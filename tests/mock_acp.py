"""Local ACP fixture: no model, credentials, network or project file access."""
import copy
import fcntl
import json
import os
from pathlib import Path
import sys
import uuid

root = Path(sys.argv[1])
profile = sys.argv[2]
attached = set()

def emit(value):
    print(json.dumps(value), flush=True)

def chunk(sid, role, identity, text):
    emit({'jsonrpc':'2.0','method':'session/update','params':{'sessionId':sid,'update':{
        'sessionUpdate':role+'_message_chunk','messageId':identity,'content':{'type':'text','text':text}}}})

for line in sys.stdin:
    request = json.loads(line)
    if 'id' not in request:
        continue
    method, params = request['method'], request.get('params',{})
    try:
        with (root/'lock').open('a') as lock:
            fcntl.flock(lock, fcntl.LOCK_EX)
            sessions = json.loads((root/'store.json').read_text()) if (root/'store.json').exists() else {}
            with (root/'requests.jsonl').open('a') as log:
                log.write(json.dumps({'method':method,'params':params})+'\n')
            if params.get('_meta'):
                raise ValueError('Only generic ACP is allowed')
            if method == 'initialize':
                capabilities = {'loadSession':True,'sessionCapabilities':{'list':{},'fork':{},'resume':{},'close':{}}}
                if profile != 'direct': capabilities['sessionCapabilities']['delete'] = {}
                result = {'protocolVersion':1,'agentCapabilities':capabilities}
            elif method == 'session/new':
                sid = str(uuid.uuid4())
                sessions[sid] = {'turns':[], 'owner':os.getpid(), 'updated':0,'title':sid}
                attached.add(sid)
                result = {'sessionId':sid}
            elif method == 'session/list':
                start = int(params.get('cursor','0'))
                keys = list(sessions)[start:start+2]
                result = {'sessions':[{'sessionId':sid,'cwd':str(root),'title':sessions[sid]['title'],
                                       'updatedAt':str(sessions[sid]['updated'])} for sid in keys]}
                if start+2 < len(sessions): result['nextCursor'] = str(start+2)
            elif method == 'session/fork':
                sid = str(uuid.uuid4())
                sessions[sid] = copy.deepcopy(sessions[params['sessionId']])
                sessions[sid]['owner'] = os.getpid()
                if profile != 'stable':
                    for turn in sessions[sid]['turns']:
                        turn['userId'], turn['agentId'] = str(uuid.uuid4()), str(uuid.uuid4())
                result = {'sessionId':sid}
            elif method in ['session/load','session/resume']:
                sid = params['sessionId']
                if sid not in sessions: raise ValueError('Unknown session')
                owner = sessions[sid].get('owner')
                if profile != 'direct' and owner and owner != os.getpid():
                    try: os.kill(owner,0)
                    except ProcessLookupError: pass
                    else: raise ValueError('Session already has an active writer')
                sessions[sid]['owner'] = os.getpid()
                attached.add(sid)
                if method == 'session/load':
                    for turn in sessions[sid]['turns']:
                        chunk(sid,'user',turn['userId'],turn['prompt'])
                        chunk(sid,'agent',turn['agentId'],'Reply: '+turn['prompt'])
                result = {}
            elif method == 'session/prompt':
                sid = params['sessionId']
                if sid not in attached: raise ValueError('Fork needs attach on the creating connection')
                prompt = ''.join(p.get('text','') for p in params['prompt'])
                turn = {'prompt':prompt,'userId':str(uuid.uuid4()),'agentId':str(uuid.uuid4())}
                sessions[sid]['turns'].append(turn)
                sessions[sid]['updated'] += 1
                answer = 'Reply: '+prompt
                chunk(sid,'agent',turn['agentId'],answer[:3])
                chunk(sid,'agent',turn['agentId'],answer[3:])
                result = {'stopReason':'end_turn'}
            elif method == 'session/delete':
                del sessions[params['sessionId']]
                result = {}
            elif method == 'session/close':
                attached.discard(params['sessionId'])
                result = {}
            else:
                raise ValueError('Unsupported method '+method)
            (root/'store.json').write_text(json.dumps(sessions))
            emit({'jsonrpc':'2.0','id':request['id'],'result':result})
    except Exception as error:
        emit({'jsonrpc':'2.0','id':request['id'],'error':{'code':-32603,'message':str(error)}})

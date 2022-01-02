from ipaddress import ip_address
import json
from flask import Flask, jsonify

from fail2ban_helper import send_command

app = Flask(__name__)

@app.route('/api/jails', methods=['GET'])
def get_jails():
    res = send_command(['status'])

    jails = []

    if res['error']:
        return json.dumps(res)

    for k, v in res['data']:
        if k == 'Jail list':
            jails = v.split(', ')
    return {'jails': jails}

@app.route('/api/jails/<jail_name>', methods=['GET'])
def get_jail(jail_name):
    data = {
        'currently_banned': 0,
        'total_banned': 0,
        'banned_ips': [],
        'ignored_ips': [],
        'findtime': 600,
        'bantime': 600,
        'maxretry': 0
    }


    res = send_command(['status', jail_name])
    if res['error']:
        return json.dumps(res)

    for k, v in res['data']:
        if k == 'Actions':
            for k2, v2 in v:
                if k2 == 'Currently banned':
                    data['currently_banned'] = v2

                if k2 == 'Total banned':
                    data['total_banned'] = v2

                if k2 == 'Banned IP list':
                    data['banned_ips'] = v2

    res = send_command(['get', jail_name, 'ignoreip'])
    if res['error']:
        return json.dumps(res)
    data['ignored_ips'] = res['data']

    res = send_command(['get', jail_name, 'findtime'])
    if res['error']:
        return json.dumps(res)
    data['findtime'] = res['data']

    res = send_command(['get', jail_name, 'bantime'])
    if res['error']:
        return json.dumps(res)
    data['bantime'] = res['data']

    res = send_command(['get', jail_name, 'maxretry'])
    if res['error']:
        return json.dumps(res)
    data['maxretry'] = res['data']

    return data

@app.route('/api/jails/<jail_name>/maxretry/<retries>', methods=['POST'])
def set_retries(jail_name, retries):
    jails = get_jails()
    if 'error' in jails.keys() or jail_name not in jails['jails']:
        return json.dumps(jails)

    res = send_command(['set', jail_name, 'maxretry', retries])
    if res['error']:
        return json.dumps(res)
    return jsonify(res['data'])

@app.route('/api/jails/<jail_name>/findtime/<time>', methods=['POST'])
def set_findtime(jail_name, time):
    jails = get_jails()
    if 'error' in jails.keys() or jail_name not in jails['jails']:
        return json.dumps(jails)

    res = send_command(['set', jail_name, 'findtime', time])
    if res['error']:
        return json.dumps(res)
    return jsonify(res['data'])

@app.route('/api/jails/<jail_name>/bantime/<time>', methods=['POST'])
def set_bantime(jail_name, time):
    jails = get_jails()
    if 'error' in jails.keys() or jail_name not in jails['jails']:
        return json.dumps(jails)

    res = send_command(['set', jail_name, 'bantime', time])
    if res['error']:
        return json.dumps(res)
    return jsonify(res['data'])

@app.route('/api/jails/<jail_name>/addignore/<ip_addr>', methods=['POST'])
def set_ignore(jail_name, ip_addr):
    try:
        ip_address(ip_addr)
    except ValueError:
        return {'error': 'Invalid IP address'}
    jails = get_jails()
    if 'error' in jails.keys() or jail_name not in jails['jails']:
        return jails

    res = send_command(['set', jail_name, 'addignoreip', ip_addr])
    if type(res['data']) == ValueError:
        return {'error': str(res['data'])}
    return jsonify(res['data'])

@app.route('/api/jails/<jail_name>/delignore/<ip_addr>', methods=['POST'])
def unset_ignore(jail_name, ip_addr):
    try:
        ip_address(ip_addr)
    except ValueError:
        return {'error': 'Invalid IP address'}

    jails = get_jails()
    if 'error' in jails.keys() or jail_name not in jails['jails']:
        return jails

    res = send_command(['set', jail_name, 'delignoreip', ip_addr])
    if type(res['data']) == ValueError:
        return {'error': str(res['data'])}
    return jsonify(res['data'])


@app.route('/api/jails/<jail_name>/ban/<ip_addr>', methods=['POST'])
def set_banned(jail_name, ip_addr):
    try:
        ip_address(ip_addr)
    except ValueError:
        return {'error': 'Invalid IP address'}
    jails = get_jails()
    if 'error' in jails.keys() or jail_name not in jails['jails']:
        return jails

    res = send_command(['set', jail_name, 'banip', ip_addr])
    if res['error']:
        return json.dumps(res)
    return jsonify(res['data'])

@app.route('/api/jails/<jail_name>/unban/<ip_addr>', methods=['POST'])
def set_unbanned(jail_name, ip_addr):
    try:
        ip_address(ip_addr)
    except ValueError:
        return {'error': 'Invalid IP address'}
    jails = get_jails()
    if 'error' in jails.keys() or jail_name not in jails['jails']:
        return jails

    res = send_command(['set', jail_name, 'unbanip', ip_addr])
    if res['error']:
        return json.dumps(res)
    return jsonify(res['data'])


@app.route('/api/unban/<ip_addr>', methods=['POST'])
def set_unbanned_global(ip_addr):
    try:
        ip_address(ip_addr)
    except ValueError:
        return {'error': 'Invalid IP address'}

    res = send_command(['unban', ip_addr])
    if res['error']:
        return json.dumps(res)
    return jsonify(res['data'])

@app.route('/api/banned', methods=['GET'])
def get_banned():
    res = send_command(['banned'])
    if res['error']:
        return json.dumps(res)
    return jsonify(res['data'])

@app.route('/api/reload', methods=['POST'])
def reload():
    res = send_command(['reload'])
    if res['error']:
        return json.dumps(res)
    return jsonify(res['data'])


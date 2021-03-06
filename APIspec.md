# API Specification

## Errors
Each API call may either return `{"error": True, "reason": ...}`
or its normal result. Check for this error every time.

## GET `/api/version`
Returns:
```json
{"version": "0.11.2"}
```
Where `0.11.2` is the current fail2ban server version.


## GET `/api/logtarget`
Returns your log target:
```json
{"logtarget": "/var/log/fail2ban.log"}
```

## GET `/api/dbfile`
Returns your db file:
```json
{"dbfile": "/var/lib/fail2ban/fail2ban.sqlite3"}
```

## GET `/api/dbpurgeage`
Returns the max age in seconds that ban history will be kept for:
```json
{"dbpurgeage": 86400}
```

## GET `/api/jails`
Returns:
```json
{"jails": []}
```
Where `jails` is the list of jail names.

## GET `/api/jails/<jail_name>`
Params:
`jail_name`: the name of the jail as a string.
Returns:
```json
{
	"currently_failed": 0,
	"total_failed": 0,
	"file_list": [],
	"currently_banned": 0,
	"total_banned": 0,
	"banned_ips": [],
	"banned_asns": [],
	"banned_countries": [],
	"banned_rirs": [],
	"ignored_ips": [],
	"findtime": 600,
	"bantime": 600,
	"maxretry": 0
}
```
Where `banned_ips` is a list of IPs currently banned
(`len(banned_ips) == currently_banned`), `ignored_ips` is the list of IPs
ignored by fail2ban, and `maxretry` is the number of failed login attempts
before an IP is banned.

## POST `/api/jails/<jail_name>/maxretry/<retries>`
Set the maximum amount of retries before an IP is jailed for jail `jail_name`.

## POST `/api/jails/<jail_name>/findtime/<time>`
Set the time window (in seconds) in which fail2ban searches for failed attempts
before an IP is jailed for jail `jail_name`.

## POST `/api/jails/<jail_name>/bantime/<time>`
Set the time (in seconds) IP addresses are banned for when they are banned.

## POST `/api/jails/<jail_name>/addignore/<ip_addr>`
Adds an IP address for fail2ban to ignore in jail `jail_name`.

## POST `/api/jails/<jail_name>/delignore/<ip_addr>`
Removes an IP address from the jail `jail_name`'s ignore list.

## POST `/api/jails/<jail_name>/ban/<ip_addr>`
Bans IP address `ip_addr` in jail `jail_name`.

## POST `/api/jails/<jail_name>/unban/<ip_addr>`
Unbans IP address `ip_addr` in jail `jail_name`.

## POST `/api/unban/<ip_addr>`
Unbans IP address globally.

## GET `/api/banned`
Returns:
```json
[
	{"jail1": []},
	{"jail2": []}
]
```
Where the keys are the jails, and the values are the list of banned IPs for each jail.

## POST `/api/reload`
This reloads all jails.

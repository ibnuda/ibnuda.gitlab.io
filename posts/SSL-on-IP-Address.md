SSL on IP Address
2018-02-01 08:48:26.315625015 UTC
Post


You know, because of $WORK and a few other things, I have to set up public facing server which serves SSL thingy.
Though we only have a single public IP with no domain attached on it, we can still use it.
Furthermore, it actually only takes a few easy steps.

1. Generate certificates.
2. Set nginx.
3. ???
4. Profit.

### Generate Certificates

I took the easiest path by using owntrack' [script](https://github.com/owntracks/tools/blob/master/TLS/generate-CA.sh)
to generate those certs and its friends.

In the script above, I just modified a few lines, for example,

```
IPLIST="255.255.255.255"
CA_ORG='/O=your.company.or.whatever/OU=generate-ca/emailAddress=something@something.com'

```

On `$IPLIST`, i just put our public IP, while on `$CA_ORG`, I just put some relevant information.

And then issued `bash generate.sh` followed by moving those certs to `/etc/nginx/ssl/` dir.

### Set nginx

I just assumed that you already have installed nginx on your server.
Okay, then I edited `/etc/nginx/sites-enabled/$PUBLIC_IP` to something along this line.
```
# file $YOUR_PUBLIC_IP
server {
	listen 			    8443;
	ssl 			    on;
	ssl_certificate 	/etc/nginx/ssl/$YOUR_PUBLIC_IP.crt;
	ssl_certificate_key /etc/nginx/ssl/$YOUR_PUBLIC_IP.key;
	server_name 		$YOUR_PUBLIC_IP;
	location / {
		proxy_pass 	http://127.0.0.1:5000/;
	}
}

```

On that `server` block, we tell nginx to

- Listen to port 8443.
- Use SSL encryption thingy.
- Use the generated certs by OwnTracks' `generate.sh`
- Name of the server is `$YOUR_PUBLIC_IP`.
- And we will redirect all requests to a running web app at port 5000.

I swear on me mum's name, mate, Nginx's configuration file is much nicer than Apache HTTPD's.
That's just a personal taste, though.

### ???
(Re)Start the nginx service / daemon.

### Profit
Fire up your browser and open `https://$YOUR_PUBLIC_IP`. You will be greeted by ssl warning.
Just accept it.
Sometimes, you have to trust and believe in yourself.

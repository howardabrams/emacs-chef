chef_server_url  'https://CHEF_IP:443'
node_name        'admin'
client_key       'admin.pem'
log_level        :info
log_location     STDOUT
validation_client_name 'chef-validator'

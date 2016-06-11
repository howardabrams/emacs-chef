# Feel free to adjust these variables based on if use are attempting
# to use Docker on a Mac, or if you have it installed locally.
CHEF_PORT=443
CHEF_IP:=`docker-machine ip`
CHEF_TMP=/tmp

# Basic Chef Server setup is done with 'start' ... once it is up and
# running, the 'client' target should allow you to have the stuff we
# need for our Chef connection work in Emacs.

pull:
	docker pull cbuisson/chef-server

start:  pull
	docker run --privileged -e CHEF_PORT=$(CHEF_PORT) --name chef-server -d -v $(CHEF_TMP)/chef-logs:/var/log -v $(CHEF_TMP)/install-chef-out:/root -p 443:443 cbuisson/chef-server

client:
	curl -Ok https://$(CHEF_IP):$(CHEF_PORT)/knife_admin_key.tar.gz
	cp .chef/.knife.rb .chef/knife.rb
	perl -pi -e "s/CHEF_IP/$(CHEF_IP)/" .chef/knife.rb
	tar xz -C .chef -f knife_admin_key.tar.gz

# Want to verify and connect to your Chef server from the command line
# with 'knife', then you you may want to run these commands to set up
# your Ruby virtual environment:

rvm:
	\curl -sSL https://get.rvm.io | bash -s stable ruby -- --ignore-dotfiles

ruby: rvm
	rvm install ruby

knife:
	rvm gemset create chef
	rvm gemset use chef
	gem install knife

verify: knife
	knife node list -c .chef/knife.rb

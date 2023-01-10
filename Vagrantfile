# -*- mode: ruby -*-
# vi: set ft=ruby :

# This is a Vagrantfile for easily setting up a virtual machine to run a TRIPS
# system fetched from github so that you can have a locally modifiable version
# of the web parser interface and TRIPS+WN lexicon/ontology browser.
#
# USAGE:
# - install VirtualBox (https://www.virtualbox.org/)
# - install Vagrant (https://www.vagrantup.com/)
# - put this file in its own directory, and go there, e.g.:
#   mkdir ~/trips
#   cd ~/trips
#   curl -O https://raw.githubusercontent.com/wdebeaum/WebParser/master/Vagrantfile
# - Run vagrant:
#   vagrant up
# This will ask you to select a trips system to use, then it will automatically
# set it up (this will take several minutes or hours, depending on the system),
# and run it.
# - In your web browser, go to:
#   http://192.168.56.27/
#
# More commands:
# - To ssh into the VM trips is running in:
#   vagrant ssh
# - To stop, start, restart, or check status of trips (within the VM):
#   sudo systemctl {stop|start|restart|status} trips
# - To rebuild trips after making source code changes (within the VM):
#   cd /trips/$TRIPS_SYSTEM/src
#   make clean
#   make
#   make install
# - To shut down the VM trips is running in (back to the host):
#   vagrant halt
# - To start it up again (this only does the setup the first time):
#   vagrant up
# - To uninstall the VM:
#   vagrant destroy
#
# Note that the /vagrant directory in the VM is synced to the directory you put
# this Vagrantfile in, so you can use it to transfer files in and out of the
# VM.


# find out which system we're using
TRIPS_SYSTEMS = %w{step cogent cabot bob drum musica cwmsreader propolis}
$trips_system = nil
if (File.exist?('TRIPS_SYSTEM')) # from this file saved by a previous run
  $trips_system = File.open('TRIPS_SYSTEM','r').readline.strip
else # from the user interactively
  puts "choose a TRIPS system to install, one of the following:"
  puts "  #{TRIPS_SYSTEMS.join(' ')}"
  print "TRIPS_SYSTEM> "
  $trips_system = $stdin.readline.strip
end
# check that it's one we support
TRIPS_SYSTEMS.include?($trips_system) or raise "invalid TRIPS_SYSTEM, must be one of #{TRIPS_SYSTEMS.join(' ')}"
# save it if necessary
File.open('TRIPS_SYSTEM', 'w').puts $trips_system unless (File.exist?('TRIPS_SYSTEM'))

# drum/bob need more than the space left over on the default ~10GB disk, so
# we make an extra disk just for trips
$extra_disk_size = nil
if (%w{drum bob}.include?($trips_system))
  $extra_disk_size = 5120 # MB (5GB)
end

Vagrant.configure(2) do |config|
  #config.vm.box = "debian/contrib-stretch64" # debian 9 with VBox synced folder
  config.vm.box = "debian/contrib-buster64" # debian 10 with VBox synced folder
  # doesn't work?
  #config.vm.network "forwarded_port", guest: 80, host: 32502

  config.vm.network "private_network", ip: "192.168.56.27"
  config.vm.provider "virtualbox" do |vb|
    # Customize the amount of memory on the VM:
    vb.memory = "4096"
    unless ($extra_disk_size.nil?)
      filename = Dir.pwd + '/trips.vdi'
      unless (File.exist?(filename))
	vb.customize ['createhd', '--filename', filename, '--size', $extra_disk_size]
      end
      vb.customize ['storageattach', :id, '--storagectl', 'SATA Controller', '--port', 1, '--device', 0, '--type', 'hdd', '--medium', filename]
    end
  end

  config.vm.provision "trips-dir", type: "shell", inline: <<-TRIPS_DIR
    mkdir /trips
    chown vagrant:vagrant /trips
  TRIPS_DIR

  unless ($extra_disk_size.nil?)
    config.vm.provision "extra-disk", type: "shell", inline: <<-HOME
      # make an ext4 filesystem directly on the disk (no partitioning needed)
      mkfs.ext4 /dev/sdb
      # add it to fstab so it gets mounted at boot
      echo "/dev/sdb /trips ext4 defaults" >>/etc/fstab
      # mount it now
      mount /trips
      # give it to the regular user
      chown vagrant:vagrant /trips
    HOME
  end

  config.vm.provision "locales", type: "shell", inline: <<-LOCALES
    set -eux
    echo "en_US.UTF-8 UTF-8" >/etc/locale.gen
    locale-gen
  LOCALES

  config.vm.provision "environment", type: "shell", privileged: false, inline: <<-ENVIRONMENT
    set -eux
    if [ ! -e /trips/env.sh ] ; then
      cat >/trips/env.sh <<-ENVSH
	# remember which TRIPS system the user selected
	export TRIPS_SYSTEM=#{$trips_system}
	export TRIPS_BASE=/trips/\\$TRIPS_SYSTEM
	# tell WebParser to allow editing of dynamic lexicon/ontology (adding WN senses to TRIPS)
	export ALLOW_LEX_ONT_EDITING=t
	# silence curl progress meters since vagrant messes them up
	# FIXME: somehow this alias sometimes doesn't apply
	alias curl='curl -sS'
ENVSH
    fi
  ENVIRONMENT

  config.vm.provision "common-packaged-deps", type: "shell", inline: <<-CPD
    set -eux
    . /trips/env.sh
    # common stuff
    # deps that are debian packages
    apt-get --allow-releaseinfo-change update
    # stretch: gcc-6 openjdk-8-jdk; buster: gcc-8 openjdk-11-jdk
    apt-get install -y \
      git \
      make \
      curl \
      unzip \
      g++-8 \
      libicu-dev \
      pkg-config \
      sbcl \
      openjdk-11-jdk \
      perl \
      ruby \
      xsltproc \
      sqlite3 \
      libdbd-sqlite3-perl \
      lighttpd \
      libcgi-pm-perl \
      graphviz \
      screen
    # make sure g++ is actually accessible as g++ and not just g++-8
    if ! which g++ ; then ln -s g++-8 /usr/bin/g++ ; fi
  CPD

  config.vm.provision "common-unpackaged-deps", type: "shell", privileged: false, inline: <<-CUD
    set -eux
    . /trips/env.sh
    sudo mkdir -p /usr/local/share
    sudo chown vagrant:vagrant /usr/local/share
    cd /usr/local/share
    # WordNet (debian does have this but it messes up the directory structure and doesn't have glosstag)
    mkdir wordnet
    cd wordnet
    curl -sS -O "https://wordnetcode.princeton.edu/3.0/WordNet-3.0.tar.bz2"
    curl -sS -O "https://wordnetcode.princeton.edu/glosstag-files/WordNet-3.0-glosstag.tar.bz2"
    tar -jxf WordNet-3.0.tar.bz2
    tar -jxf WordNet-3.0-glosstag.tar.bz2
    cd ..
    # Stanford CoreNLP
    mkdir stanford-corenlp
    cd stanford-corenlp
    curl -sS -O -L "http://nlp.stanford.edu/software/stanford-corenlp-4.2.0.zip"
    unzip stanford-corenlp-4.2.0.zip
  CUD

  config.vm.provision "system-specific", type: "shell", privileged: false, inline: <<-SS
    set -eux
    . /trips/env.sh
    # system-specific stuff
    cd /trips
    git clone https://github.com/wdebeaum/$TRIPS_SYSTEM.git
    cd $TRIPS_SYSTEM/src/
    # name of the trips run script
    TRIPS_EXE=trips-$TRIPS_SYSTEM
    # arguments to the trips run script
    TRIPS_ARGS='-nouser'
    # extra debian packages to install
    packages=''
    # EKB has a bunch of these, so group them here
    ekb_packages='libfile-slurp-perl libxml-libxml-perl libtest-deep-perl libxml2-utils'
    # whether to install geonames data
    geonames=''
    # whether to install enju parser
    enju=''
    # whether to install Node.js/NPM
    # (debian has node, but stretch doesn't have npm, and buster's npm is
    # busted, so we just bypass debian and get it straight from the source)
    node=''
    # set the above variables according to $TRIPS_SYSTEM
    case "$TRIPS_SYSTEM" in
      step) TRIPS_ARGS='-display none'; packages='python'; geonames=y;;
      cogent) packages='aspell-en';;
      cabot) packages='python aspell-en';;
      bob) packages="aspell-en libset-scalar-perl $ekb_packages"; geonames=y; enju=y;;
      drum) packages="libset-scalar-perl $ekb_packages" geonames=y; enju=y;;
      musica) ;;
      cwmsreader)
        packages="python python-pip virtualenv aspell-en $ekb_packages"
	geonames=y
	node=y
	# avoid JavaFX on java 11/buster (don't need it for web stuff)
	mv $TRIPS_BASE/src/util/cwc/Makefile-java $TRIPS_BASE/src/util/cwc/Makefile-java.disabled
	mv $TRIPS_BASE/src/PDFExtractor $TRIPS_BASE/src/PDFExtractor.disabled
	mv $TRIPS_BASE/src/DocumentRepo $TRIPS_BASE/src/DocumentRepo.disabled
	;;
      propolis) packages="aspell-en $ekb_packages";;
      *) echo "unknown system, no system-specific deps installed (you're on your own)" ;;
    esac
    echo "export TRIPS_EXE='$TRIPS_EXE'" >>/trips/env.sh
    echo "export TRIPS_ARGS='$TRIPS_ARGS'" >>/trips/env.sh
    # install deps as needed
    if [ -n "$enju" ] ; then
      packages="$packages zlib1g-dev"
    fi
    if [ -n "$packages" ] ; then
      sudo apt-get install -y $packages
    fi
    if [ -n "$enju" ] ; then
      cd /usr/local/share/
      # this is where configure looks for enju, so we'll install it there
      mkdir -p enju/enju-2.4.2/
      cd enju
      git clone --depth=1 https://github.com/mynlp/enju
      cd enju
      ./configure --prefix=/usr/local/share/enju/enju-2.4.2
      make
      make install
      cd /usr/local/share/enju/enju-2.4.2
      ln -s bin/enju ./
    fi
    if [ -n "$geonames" ] ; then
      geonames_dir=/usr/local/share/geonames/`date +%Y-%m-%d`
      mkdir -p $geonames_dir
      cd $geonames_dir
      curl -sS -O "https://geonames.usgs.gov/docs/stategaz/NationalFile.zip"
    fi
    if [ -n "$node" ] ; then
      node_dir=/usr/local/share/node
      mkdir -p $node_dir
      cd $node_dir
      curl -sS -O "https://nodejs.org/dist/v10.15.3/node-v10.15.3-linux-x64.tar.xz"
      tar -Jxf node-v10.15.3-linux-x64.tar.xz
      cd /usr/local/bin
      sudo ln -s $node_dir/node-v10.15.3-linux-x64/bin/* ./
    fi
  SS

  config.vm.provision "common-setup", type: "shell", privileged: false, inline: <<-CS
    set -eux
    . /trips/env.sh
    # more common stuff
    mkdir -p $TRIPS_BASE/logs
    cd $TRIPS_BASE/src/
    # WebParser
    if [ ! -e WebParser ] ; then
      git clone https://github.com/wdebeaum/WebParser.git
    fi
    # overwrite lighttpd config
    sudo bash -c "cat >/etc/lighttpd/lighttpd.conf" <<-LIGHTTPDCONF
	server.modules=("mod_cgi")
	\\$HTTP["url"] =~ "^/cgi" { cgi.assign = ( "" => "" ) }
	server.document-root="$TRIPS_BASE/www"
	server.bind="192.168.56.27"
	mimetype.assign = (
	  ".html" => "text/html",
	  ".css" => "text/css",
	  ".js" => "text/javascript",
	  ".xsl" => "application/xml",
	  ".xml" => "application/xml"
	)
	index-file.names=("index.html")
LIGHTTPDCONF
  CS

  config.vm.provision "build", type: "shell", privileged: false, keep_color: true, inline: <<-BUILD
    set -eux
    . /trips/env.sh
    # main build
    cd $TRIPS_BASE/src/
    ./configure --with-lisp=sbcl --with-corenlp=/usr/local/share/stanford-corenlp/stanford-corenlp-4.2.0/
    make
    make install
    cd WebParser/
    make -f Makefile-component install
    # make sure the cgi script for the chosen system is really installed (step
    # breaks the pattern):
    ./install-cgi.pl $TRIPS_BASE/www/cgi $TRIPS_SYSTEM
    # also add an index page:
    cat >$TRIPS_BASE/www/index.html <<-INDEX
      <html>
      <head><title>Your local instance of TRIPS/$TRIPS_SYSTEM</title></head>
      <body><h1>Your local instance of TRIPS/$TRIPS_SYSTEM</h1><ul>
      <li><a href="cgi/lex-ont">TRIPS+WN Lexicon/Ontology Browser</a>
      <li><a href="cgi/$TRIPS_SYSTEM">TRIPS/$TRIPS_SYSTEM Parser Web Interface</a>
      </ul></body>
      </html>
INDEX
  BUILD

  # set up an init script that runs the system and web server
  config.vm.provision "init-script", type: "shell", inline: <<-INIT
    set -eux
    . /trips/env.sh
    cat >/etc/init.d/trips <<-TRIPS
#!/bin/bash
### BEGIN INIT INFO
# Provides:		trips
# Required-Start:	lighttpd \\$local_fs \\$time
# Required-Stop:	lighttpd \\$local_fs \\$time
# Default-Start:	2 3 4 5
# Default-Stop:		0 1 6
# Short-Description:	TRIPS web parser and lexicon/ontology browser
### END INIT INFO
. /trips/env.sh
case "\\$1" in
  start)
    echo "starting trips"
    cd \\$TRIPS_BASE/logs
    sudo -u vagrant screen -d -m \\$TRIPS_BASE/bin/\\$TRIPS_EXE \\$TRIPS_ARGS
    ;;
  stop)
    echo "stopping trips"
    $TRIPS_BASE/bin/trips_msg request :receiver facilitator :content '(exit)'
    ;;
  restart|force-reload)
    /etc/init.d/trips stop ; sleep 5 && /etc/init.d/trips start
    ;;
  status)
    if ! \\$TRIPS_BASE/bin/trips_msg request :receiver facilitator :content '(get-status)' ; then
      echo "trips is not started"
      exit 1
    fi
    ;;
  *)
    echo "Usage: /etc/init.d/trips {start|stop|restart|force-reload|status}"
    exit 1
    ;;
esac
exit 0
TRIPS
    chmod 755 /etc/init.d/trips
    update-rc.d trips defaults
  INIT

  # run the init script we just set up
  config.vm.provision "run", type: "shell", inline: <<-RUN
    systemctl reload lighttpd
    systemctl start trips
  RUN
end

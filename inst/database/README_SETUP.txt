Setting up database



good blog:
https://savvinov.com/2020/07/13/installing-r-shiny-server-on-oci-compute-instance-always-free-tier/



|> Database homepage
|> Network section
| |> Turn off Mutual TLS (mTLS) authentication (set to Not required)
| |> Add the IP addresses (of your home computer and the Oracle instance) to `Access control list:`


Oracle cloud infrastructure
- make VNC network for the shiny server
    - main page, left-hand side menu: Networking -> Virtual Cloud Networks -> Start VCN Wizard
- make an instance
    - Compute -> Instances -> Create Instance

Setup server
- login via terminal (or setup ssh keys): ssh -i opc@ip_address_from_prev_step
- install R: sudo yum install R
- install r-shiny: sudo su - -c "R -e \"install.packages('shiny',repos='https://cran.rstudio.com/')\""
    - also install other R packages like this
- go to Rstudio and download the R Studio Server then install
- auto booting of server: sudo systemctl enable shiny-server
- start: sudo systemctl start shiny-server
- open firewall:
    - sudo firewall-cmd --permanent --add-port=3838/tcp
    - sudo firewall-cmd --permanent --add-port=3838/udp
    - sudo firewall-cmd --reload
- sudo yum install cairo-devel
- sudo su - -c "R -e \"install.packages('Cairo', repos='https://cran.rstudio.com/')\""

Get the package ready:
- create with golem
- make sure to run golem::add_shinyserver_file()
- push to github
- on the server go to /srv/shiny-server/
- sudo git clone package

Set the shiny_server config
- /etc/shiny-server/shiny-server.conf
- turn index off
- ensure paths correct
- restart the server: sudo systemctl restart shiny-server

See the app:
ip_address:3838/package_name




some helpers to add to server .bashrc file:

alias sourcebash='source ~/.bashrc'
alias editbash='nano ~/.bashrc'
alias home='cd /srv/shiny-server/hfphenotyping/'
alias restart='sudo systemctl restart shiny-server'
alias config='sudo nano /etc/shiny-server/shiny-server.conf'
alias logs='cd /var/log/shiny-server/'
log() {
    latest_log=$(ls -t /var/log/shiny-server/*.log | head -n 1)
    echo "Tailing the latest log file: $latest_log"
    sudo cat "$latest_log"
}







=====================

Java on Oracle

Download Java for the operating system e.g. https://adoptium.net/en-GB/ or from Java site.
Install
run sudo R CMD javareconf
then reinstall rJava in R

[root@heart-failure-phenotyping include]# sudo update-alternatives --config java

There is 2 program that provides 'java'.

  Selection    Command
-----------------------------------------------
*+ 1           /usr/lib/jvm/jre-1.8-oracle-x64/bin/java
   2           java-1.8.0-openjdk.x86_64 (/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.412.b08-2.0.1.el8.x86_64/jre/bin/java)

Enter to keep the current selection[+], or type selection number: 2
[root@heart-failure-phenotyping include]# sudo R CMD javareconf
Java interpreter : /bin/java
Java version     : 1.8.0_412
Java home path   : /usr/lib/jvm/java-1.8.0-openjdk-1.8.0.412.b08-2.0.1.el8.x86_64/jre
Java compiler    : /bin/javac
Java headers gen.: /bin/javah
Java archive tool: /bin/jar

trying to compile and link a JNI program
detected JNI cpp flags    : -I$(JAVA_HOME)/../include -I$(JAVA_HOME)/../include/linux
detected JNI linker flags : -L$(JAVA_HOME)/lib/amd64/server -ljvm
gcc -m64 -I"/usr/lib64/R/../../include/R" -DNDEBUG -I/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.412.b08-2.0.1.el8.x86_64/jre/../include -I/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.412.b08-2.0.1.el8.x86_64/jre/../include/linux  -I/usr/local/include   -fpic  -g -O2  -c conftest.c -o conftest.o
gcc -m64 -shared -L/usr/lib64/R/lib -L/usr/local/lib64 -o conftest.so conftest.o -L/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.412.b08-2.0.1.el8.x86_64/jre/lib/amd64/server -ljvm -L/usr/lib64/R/lib -lR


JAVA_HOME        : /usr/lib/jvm/java-1.8.0-openjdk-1.8.0.412.b08-2.0.1.el8.x86_64/jre
Java library path: $(JAVA_HOME)/lib/amd64/server
JNI cpp flags    : -I$(JAVA_HOME)/../include -I$(JAVA_HOME)/../include/linux
JNI linker flags : -L$(JAVA_HOME)/lib/amd64/server -ljvm
Updating Java configuration in /usr/lib64/R
Done.

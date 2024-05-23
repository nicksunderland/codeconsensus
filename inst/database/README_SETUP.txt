Java on Oracle

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

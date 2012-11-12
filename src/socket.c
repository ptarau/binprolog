/*
 *  Module   :  TCP client+server - based on free code from SunSolve,
 *              GNU sockets, Unix socket FAQ examples by Vic Metcalfe.
 *              Changes (well, a lot of them) by Paul Tarau:
 *                   binnetcorp@binnetcorp.com
*/

#include <stdio.h>

#define MAX_WAITING_CLIENTS 1024
#define MAX_CLIENT_TRY 2

#ifdef STANDALONE_SOCKET_STUFF
#if VCC>0 && (defined _M_AMD64)
#define WSIZE 64
#else
#define WSIZE 32
#endif

#if WSIZE == 64
typedef long long bp_long;
typedef unsigned long long cell;
#define LFORMAT "%lld"
#define UFORMAT "%llu"
#else
typedef long bp_long;
typedef unsigned long cell;
#define LFORMAT "%ld"
#define UFORMAT "%lu"
#endif

#define SERV_PORT  7001
#define MAXBUF     1<<17

/* increment QLEVEL for less verbose messages e.g to 2*/
#define QLEVEL() 0
#define STD_in stdin
#define STD_out stdout
#define STD_err stderr

bp_long fsize(FILE *cf);

#ifndef VCC
#define VCC 0
#endif

#else
/* not STANDALONE_SOCKET_STUFF */
#include "global.h"

extern struct limit max;
#define MAXBUF max.SBUF
extern bp_long fsize(FILE *cf);
extern int unix_sleep(int n);
#endif


#define TRACE_BELOW(Level) if(QLEVEL()<=(Level))

static void io_trace1(char *mes) {
  TRACE_BELOW(0)
  fprintf(STD_err,"%s\n",mes);
}

static void io_trace_sn(char *mes,bp_long n) {
  TRACE_BELOW(0)
  fprintf(STD_err,"%s: %ld\n",mes,(long)n);
}

static void io_trace_snn(char *mes,bp_long n,bp_long m) {
  TRACE_BELOW(0)
  fprintf(STD_err,"%s: %ld, %ld\n",mes,(long)n,(long)m);
}

static void io_trace_ss(char *mes,char *val) {
  TRACE_BELOW(0)
  fprintf(STD_err,"%s: %s\n",mes,val);
}

static void perror0(char *mes) {
  TRACE_BELOW(1)
  perror(mes);
}

static void io_mes0(char *mes) {
  TRACE_BELOW(2)
  printf("%s",mes);
}



#if 0

/* stubs replacing real code if socks do not work */

static bp_long todo(char *s) {
  fprintf(STD_err,"%s() unimplemented for this platform",s);
  exit(100);
  return -1;
}

#define TODO(What) {return todo(What);}
bp_long new_client(char *host, bp_long port) TODO("new_client")
bp_long new_server(bp_long port) TODO("new_server")
bp_long new_service(bp_long sock,bp_long t) TODO("new_service")
bp_long sock_readln(bp_long sock,char *buf,cell limit) TODO("sock_readln")
bp_long sock_writeln(bp_long sock,char *buf,bp_long l0) TODO("sock_writeln")
bp_long sock_read(bp_long sock,char *buf,bp_long limit) TODO("sock_read")
bp_long sock_write(bp_long sock,char *buf,bp_long l0) TODO("sock_write")
char *peer_addr(bp_long socket) {return "127.0.0.1";}
bp_long peer_port(bp_long socket) TODO("peer_port")
bp_long close_socket(bp_long sock) TODO("close_socket")
bp_long file2sock(FILE *from, bp_long sock,char *buf) TODO("file2sock")
bp_long sock2file(bp_long sockfd, FILE *to, char *buf) TODO("sock2file")
char *host2ip(char *hostname) {return "127.0.0.1";}
char *get_ip_addr() {return "127.0.0.1";}
char *get_host() {return "127.0.0.1";}

#else
#include <sys/types.h>
#include <errno.h>
#include <signal.h>
#include <string.h>
#if(!VCC)
#include <unistd.h>
#endif

#if VCC>0
#include <winsock.h>
bp_long close_socket(bp_long sock) {
  io_trace_sn("closing socket",sock);
  return closesocket(sock);
}

#define INIT_WINSOCK() init_winsock()
#define ECONNREFUSED WSAECONNREFUSED
#define MAXHOSTNAMELEN 256
void init_winsock() {
  static bp_long done=0;
  if(!done) {
    WSADATA            wsadata;
    WSAStartup(0x0101,&wsadata);
    done=1;
  }
}
#define TOP_PROTOCOL SOL_SOCKET
#else
/* dummy def to let trace on Unix Windows stuff */
bp_long close_socket(bp_long sock) {
  io_trace_sn("closing socket",sock);
  return close(sock);
}
#define TOP_PROTOCOL SOL_SOCKET
#define INIT_WINSOCK()
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/param.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h> /* rlimit */
#include <unistd.h>
#if (VCC==0)
#include <netinet/tcp.h> /* will break WINGCC - just comment it out */
#endif
char *inet_ntoa();
extern unsigned long inet_addr(char *name);
extern int errno;
#define WSAGetLastError() errno
#endif

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 256
#endif

char *get_host() {
  static bp_long state=0; bp_long ok;
  static char host_name[MAXHOSTNAMELEN];
  if(0==state) {
    state=1;
	INIT_WINSOCK();
    ok=gethostname(host_name,MAXHOSTNAMELEN);
    if(ok<0) {strcpy(host_name,"localhost");
      perror0("host not found by gethostname(), localhost assumed");
    }
  }
  return host_name;
}


char *host2ip(char *hostname) {
  char *ip_addr;
  struct hostent *hostptr;
  struct in_addr *hostnameptr;
  INIT_WINSOCK();
  hostptr = gethostbyname(hostname);
  if(NULL==hostptr) {
      perror0("ip addr not found, 127.0.0.1 assumed");
      ip_addr="127.0.0.1";
  }
  else {
     hostnameptr = (struct in_addr *) hostptr->h_addr_list[0];
     ip_addr=inet_ntoa(*hostnameptr);
  }
  return ip_addr;
}

char *get_ip_addr() {
  static bp_long state=0;
  static char ip_addr[MAXHOSTNAMELEN];

  if(0==state) {
    strcpy(ip_addr,host2ip(get_host()));
    state=1;
  }

  return ip_addr;
}

char *peer_addr(bp_long socket) {
  static char peer_addr[MAXHOSTNAMELEN];
  struct sockaddr_in sa; unsigned int namelen;
  namelen = sizeof (sa);
  if (0!=getpeername(socket, (struct sockaddr *)&sa, &namelen))
    return NULL;
  strcpy(peer_addr, inet_ntoa(sa.sin_addr));
  return peer_addr;
}

bp_long peer_port(bp_long socket) {
  struct sockaddr_in sa; unsigned int namelen; bp_long res;
  namelen = sizeof (sa);
  if (0!=getpeername(socket, (struct sockaddr *)&sa, &namelen))
    return -1;
  res=ntohs(sa.sin_port);
  /*fprintf(STD_err,"!!!RES:%d\n",res);*/
  return res;
}


/*
*
*  Tools:
*
*/



/* This function reads from a socket, until it recieves a linefeed
   character.  It fills the buffer "str" up to the maximum size "count".

   This function will return -1 if the socket is closed during the read
   operation.

   Note that if a single line exceeds the length of count, the extra data
   will be read and discarded!  You have been warned.


#13,#10 = DOS
#13 = MAC
#10 = UNIX

*/

/*static char *eol="\13\10";*/
static char eol[]={13,10,0}; /* 13 will be ignored */

#define AT_EOL(aChar) (aChar==eol[1] || aChar==eol[2])

static bp_long sock_gets(bp_long sockfd, char *str, size_t count)
{
  bp_long bytes_read;
  char *current_position = str;
  char last_read = (char)-1;

  while (!AT_EOL(last_read)) {
#if 0
    fprintf(STD_err,"sock_gets:last_read>>%d<<\n",last_read);
#endif
#if VCC>0
    bytes_read = recv(sockfd, &last_read, 1, 0);
#else
    bytes_read = read(sockfd, &last_read, 1);
#endif
    if (bytes_read <= 0) {
      /* The other side may have closed unexpectedly */
      return -1; /* Is this effective on other platforms than linux? */
    }
    if ( (current_position-str < (bp_long)count) && !AT_EOL(last_read)) {
      *current_position = last_read;
      current_position++;
    }
  }
  /* cleans up possible end of line chars */
  if (count > 0) {
    *current_position = 0;
  }
  if(current_position>str && *(current_position-1)==eol[0]) {
	   *(current_position-1)=0;
  }
#if 0
  fprintf(STD_err,"sock_read TRACE [%d]>>%s<<\n",current_position-str,str);
#endif
  return current_position-str;
}

/*
* Reads a line terminated with \n or \0 from a socket
*/
bp_long sock_readln(bp_long sock,char *buf,cell limit) {
   bp_long ok;
   if(0==limit) limit=MAXBUF;
   else if(limit>MAXBUF) limit=MAXBUF;

   buf[0]='\0';
   io_trace_ss("sock_readln start: Reading",buf);
   ok=sock_gets(sock,buf,limit)>=0;
   io_trace_ss("sock_readln end: Reading",buf);
   /*
   {bp_long i,len;
     len=strlen(buf);
     for(i=(len>=3)?(len-3):0;i<len;i++) {
       if('.'==buf[i]) buf[i]=' ';
     }
   }
   */
   return ok;
}

/* This is just like the read() system call, accept that it will make
   sure that all your data goes through the socket. */

bp_long sock_read0(bp_long sockfd, char *buf, size_t count)
{
  size_t bytes_read = 0;
  bp_long this_read;

  while (bytes_read < count) {
    do {
#if VCC>0
      this_read = recv(sockfd, buf, count - bytes_read,0);
#else
      this_read = read(sockfd, buf, count - bytes_read);
#endif
      io_trace_snn("sock_read0, sock, this!!",sockfd,this_read);
    }
    while ( (this_read < 0) && (errno == EINTR) ); /* end do */

    if (this_read <= 0) break;

    bytes_read += this_read;
    buf += this_read;
  }
  return bytes_read;
}

/*
Strange enough, this should be u_int and not u_long, for compatibility
with 64 bit DEC Alphas where ntohl actually means ntoh-32 bit bp_long.
Fine too on 32 bit machines where usually sizeof(bp_long)=sizeof(bp_long)
 */

#define ADDR_T unsigned
#define USIZE() sizeof(ADDR_T)

static ADDR_T get_charno(char *magic) {
  return ntohl(*(ADDR_T *) magic);
}

static void set_charno(char *magic, ADDR_T l) {
  ADDR_T *addr=(ADDR_T *)magic;
  *addr=htonl(l);
}

bp_long sock_read(bp_long sockfd, char *buf,bp_long limit) {
  bp_long count; ADDR_T l;

  if(0==limit) { /* size is the firs bp_long in data */
    char magic[USIZE()];
    count=sock_read0(sockfd, magic, USIZE());
    if(count!=USIZE()) return 0;
    l=get_charno(magic);
  }
  else /* size is given by user */
    l=(ADDR_T)limit;

  if(l>=MAXBUF) {
	 perror0("sock_read: buffer overflow");
	 return 0;
     /* in fact, this means max.SBUF, a BinProlog limit */
  }
  buf[0]='\0';
  count=sock_read0(sockfd, buf, l);
  if(count>=0) buf[count]='\0';
  io_trace_ss("sock_read: Reading",buf);
  return count;
}

bp_long sock2file(bp_long sockfd, FILE *to, char *buf) {
  bp_long ok; ADDR_T l;
  char magic[USIZE()];
  io_trace_snn("sock2file: Try reading sock, file stream ",sockfd,(bp_long)to);
  ok=sock_read0(sockfd, magic, USIZE());
  if(ok!=USIZE()) return 0;
  l=get_charno(magic);
  while(l>0) {
    ok=(l<MAXBUF)?l:MAXBUF;
    ok=sock_read0(sockfd, buf, ok);
    if(ok<0) return 0;
    l-=ok;
    ok=fwrite(buf, 1, ok, to);
    io_trace_snn("sock2file: Read from, bytes:",sockfd,ok);
  }
  return ok>=0 && !ferror(to);
}


/* This is just like the write() system call, accept that it will
   make sure that all data is transmitted. */


bp_long sock_write0(bp_long sockfd, char *buf, size_t count)
{
  size_t bytes_sent = 0;
  bp_long this_write;

  io_trace_snn("sock_write0: trying to write, sock, count",sockfd,count);

  while (bytes_sent < count) {
    do
#if VCC>0
      this_write = send(sockfd, buf, count - bytes_sent,0);
#else
      this_write = write(sockfd, buf, count - bytes_sent);
#endif
    while ( (this_write < 0) && (errno == EINTR) );
    if (this_write <= 0) {
      io_trace_snn("sock_write0: written, sock, count",sockfd,bytes_sent);
      return this_write;
    }
    bytes_sent += this_write;
    buf += this_write;
  }
  io_trace_snn("sock_write0: written, sock, count",sockfd,bytes_sent);
  return count;
}

/*
bp_long sock_write(bp_long sock, char *buf,bp_long option) {
   bp_long ok;
   ADDR_T l;
   if(0==option) {
     char magic[USIZE()];
     l=strlen(buf);
     set_charno(magic,l);
     ok=sock_write0(sock,magic,USIZE());
     if(ok!=USIZE()) return 0;
   }
   else
     l=option;
   ok=sock_write0(sock,buf,l);
   if(ok>=0) io_trace_ss("sock_write: written",buf);
   buf[0]='\0';
   return ok>=0;
}
*/

bp_long sock_write(bp_long sock, char *buf,bp_long option) {
   bp_long ok;
   ADDR_T l;
   char magic[USIZE()];
   if(0==option) {
   l=strlen(buf);
   }
   else {
   l=option;
   }
   set_charno(magic,l);
   ok=sock_write0(sock,magic,USIZE());
   //io_trace_snn("sock_write, l, USIZE ",l,USIZE());
   if(ok!=USIZE()) return 0;
   ok=sock_write0(sock,buf,l);
   //io_trace_ssnn("sock_write, buffer, socket, l ",buf,sock,l);
   if(ok>=0) io_trace_ss("sock_write: written",buf);
   buf[0]='\0';
   return ok>=0;
}

bp_long file2sock(FILE *from, bp_long sock,char *buf) {
   bp_long ok; ADDR_T l; ADDR_T s;
   char magic[USIZE()];
   s=fsize(from);
   set_charno(magic,s);
   ok=sock_write0(sock,magic,USIZE());
   if(ok!=USIZE()) return 0;
   while(s>0) {
     l=fread(buf,1,MAXBUF,from);
     if(l<=0) {
       io_trace_snn("file2sock: bad file stream, code",(bp_long)from,l);
       return 0;
     }
     io_trace_snn("file2sock: read from, chars",(bp_long)from,l);
     s-=l;
     ok=sock_write0(sock,buf,l);
     if(ok>=0) io_trace_snn("file2sock: written from file stream, sock",
                             (bp_long)from,sock);
     buf[0]='\0';
   }
   return ok>=0;
}




/*
* Writes out a line to a socket
*/
bp_long sock_writeln(bp_long sock,char *buf,bp_long option) {
   bp_long ok;
   bp_long l=strlen(buf);
   if(0==l || (l>0 && buf[l-1]!=eol[0] && buf[l-1]!=eol[1]))
     {
       strcat(buf,eol);
       l+=strlen(eol);
     }
   ok=sock_write0(sock, buf,l);
   buf[0]='\0';
   return ok>=0;
}


/*  PART I:     client:
 *  Function :  This client program demonstrates how to open the TCP
 *              socket, connect to the TCP server via predetermined unique
 *              server port number.  Then, it sends data to the server via
 *              interactive user input from the keyboard.
 *
 */


/*
*  Creates a new client connected to host, port.
*/

bp_long new_client0(char *host, bp_long port) /* starts client */ {

    bp_long  s;                       /* socket file descriptor */
#if 0==VCC
    struct hostent *gethostbyname();
#endif
    struct hostent *hp;
    struct sockaddr_in peeraddr_in;
    bp_long  on = 1;

    io_trace_ss(
        "Entering new_client for host", host);
    io_trace_sn(
        "  new_client server port", port);


    INIT_WINSOCK();

    /* Find information for the remote host */

#if VCC>0
    { static cell l; char *h=(char *)&l;

      l=inet_addr(host);
      io_trace_sn("got l",l);

      if ((bp_long)l<0 && (hp = gethostbyname(host) ) == NULL) {
        io_trace_ss(
        "Neither an IP address nor host found by name", host);
        io_trace_sn("errorcode:", WSAGetLastError());
        return -1;
      }

      TRACE_BELOW(0)
      {  char buf[256];
         sprintf(buf,"%u.%u.%u.%u",h[0],h[1],h[2],h[3]);
         io_trace_ss("gethostby...() got addr",buf);
      }

      if((bp_long)l<0) {
         io_trace_ss("Host found by name", host);
      }
      else {
        /*try out if this host makes sense anyway ...
        by cloning a valid "localhost" based hostent hp */

        if((hp = gethostbyname("localhost") ) == NULL) {
          io_trace_sn("no localhost neither, error",WSAGetLastError());
          return -1;
         }

         hp->h_addr=host;
         hp->h_length=strlen(host);
         *hp->h_addr_list=h;

         io_trace_ss("host assumed valid IP address", host);
      }
    }
#else
    if ((hp = gethostbyname(host) ) == NULL) {
        ADDR_T addr=inet_addr(host);
        if(addr<0) {
          io_trace_ss(
            "Bad IP address or host name in gethostbyname", host);
          return -1;
        }
        io_trace_sn("gethostbyname() did not work but got addr",addr);

        if ((hp = gethostbyaddr((char *)&addr,sizeof(addr),AF_INET) )
           == NULL) {
             io_trace_ss("Host not found by gethostbyaddr", host);
             return -1;
        }
        io_trace_sn("gethostbyaddr() got addr",addr);
    }
#endif

    /* clear out the address structure */

    memset((char *)&peeraddr_in, 0, sizeof(peeraddr_in));

    /* Set up peer address for connect */

    peeraddr_in.sin_family = AF_INET;

    memcpy((char *)&peeraddr_in.sin_addr, hp->h_addr, hp->h_length);

    peeraddr_in.sin_port = htons((short)port);

    /* Create the socket */

    if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    {
        perror0("Unable to create socket");
        return -1;
    }

    if(connect(s, (struct sockaddr *)&peeraddr_in,
                      sizeof(peeraddr_in)) < 0)    {
        if(errno != ECONNREFUSED)
        {
            perror0("Connection attempt failed");
        }
        return -1;
    }


#if defined SO_KEEPALIVE
    if (setsockopt(s, TOP_PROTOCOL, SO_KEEPALIVE,
               (char *)&on, sizeof(on)) < 0)   {
        perror0("Cannot set KEEPALIVE");
        return -1;
    }
#endif



#if defined TCP_NODELAY
    if (setsockopt(s, IPPROTO_TCP, TCP_NODELAY,
               (char *)&on, sizeof(on)) < 0)   {
        perror0("Cannot set TCP_NODELAY");
        return -1;
    }
#endif


    io_trace_ss(
        "Exiting new_client for host", host);
    io_trace_sn(
        "  new_client server port", port);
    io_trace_sn(
        "  new_client socket", s);

    return s;
}

static void sock_sleep(bp_long secs) {
#ifndef STANDALONE_SOCKET_STUFF
        unix_sleep(secs);
#else
#if VCC>0
        _sleep((secs)*1000);
#else
        sleep(secs);
#endif
#endif
}

bp_long new_client(char *host, bp_long port) {
   bp_long sock= -1;
   int i;
   for(i=0; i<MAX_CLIENT_TRY; i++) {
     sock =new_client0(host,port);
     if(sock>0) break;
     sock_sleep(1<<i);
     { char buf[128];
       sprintf(buf,
         "%s %s:%d\n%s%d %s%d %s\n",
         "*** error opening socket to",host,(int)port,
         "no answer or no available sockets, retry=",
         i,"waiting=",1<<i,"seconds");
       io_mes0(buf);
     }
   }
   return sock;
}

/* end TCP client */

/*
 *  PART II  :  server
 *  Function :  This server program demonstrates how to open the TCP
 *              socket, bind to a local transport address, listen on the,
 *              and ready to accept the connection from any TCP client.
 *              It reads the data sent from the client and answers back.
 *              Then, go back to accept a new
 *              client connection.
 */

/*
*  Creates a new server listening on port
*/

bp_long new_server(bp_long port)
{
    bp_long sock;
    struct sockaddr_in server;
    bp_long on=1;

    /* Create socket */

    INIT_WINSOCK();

    if ((sock = socket (AF_INET, SOCK_STREAM, 0)) < 0)
    {
        io_trace1("Open Stream Socket failed!");
        return 0;
    }

    if(0==sock) {
      io_trace1("Socket number not expected to be 0");
      return 0;
    }

#if VCC==0
    if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR,
        (char *)&on, sizeof(on)) < 0)    {
        io_trace1("Set SO_REUSEADDR failed!");
        return 0;
    }
#endif

    memset ((char *) &server, 0, sizeof (server));

    server.sin_family = AF_INET;
    server.sin_port = htons((short)port);
    server.sin_addr.s_addr = htonl(INADDR_ANY);


    if (bind (sock, (struct sockaddr *)&server, sizeof (server)) < 0)
    {
        io_trace_snn("Unable to bind stream socket, on port",sock,port);
        return 0;
    }

    /* listen on the socket */

    if (listen(sock, MAX_WAITING_CLIENTS) < 0)
    {
          io_trace1("Unable to listen on socket\n");
          return 0;
    }

    io_trace_sn("New server on port",port);

    return sock;
}

/*
*  OS-supported efficient wait on available file descriptor
*/

#if defined AIX
#include <sys/select.h>
#endif
bp_long wait_on_socket(bp_long sock,bp_long t) {
      bp_long sd=0;
      fd_set read_fd;
      struct timeval timeout;
      timeout.tv_sec = t;
      timeout.tv_usec= 0;

      FD_ZERO(&read_fd);
      FD_SET(sock, &read_fd);

      if ((sd=select(FD_SETSIZE, &read_fd, NULL, NULL,
           (t>0)?(&timeout):NULL)) < 0) {
           io_trace_sn("error on select",sd);
           sd=0;
      }
      else if(0==sd) {
           io_trace_snn("timeout (secs), on socket",t,sock);
           /*sd=0;*/
      }
      return sd;
}


/*
* Creates a service to chat with a client waiting on a server socket
*/
bp_long new_service(bp_long sock,bp_long t) {
      bp_long sd=0;
      bp_long on=1;
      struct sockaddr_in peeraddr;
      unsigned int addrlen=sizeof(peeraddr);

      sd=wait_on_socket(sock,t);
      if(!sd) return 0;

      io_trace_sn("Ready to accept on socket",sock);

      if ((sd = accept(sock, (struct sockaddr *)&peeraddr,
                                                 &addrlen)) < 0) {
           perror0("Accept failed");
           return 0;
      }


#if defined SO_KEEPALIVE
      if (setsockopt(sd, TOP_PROTOCOL, SO_KEEPALIVE,
                        (char *)&on, sizeof(on)) < 0) {
           close_socket(sd);
           perror0("Set SO_KEEPALIVE failed");
           return 0;
      }
#endif



#if defined TCP_NODELAY
       if (setsockopt(sd, IPPROTO_TCP, TCP_NODELAY,
                        (char *)&on, sizeof(on)) < 0) {
           close_socket(sd);
           perror0("Set TCP_NODELAY failed");
           return 0;
      }
#endif


#if 0
      io_mes_sns("peer socket socket from port, host ",
        peer_port(sd),peer_addr(sd) );
#endif

      return sd;
}

#if defined(STANDALONE_SOCKET_STUFF)

#include <sys/stat.h>

bp_long fsize(FILE *cf)
{
  struct stat cbuf;
  if (-1==fstat(fileno(cf),&cbuf)) perror0("bad file ar in fsize/1");
  return cbuf.st_size;
}

void err_exit(bp_long errno,char *mes) {
   perror(mes);
   exit(errno);
}

void catcher(bp_long a)
{
    err_exit(2,"Interrupt caught");
}

void catcher1(bp_long a)
{
    err_exit(2,"Broken Pipe caught");
}

void init_sigs() {
#if VCC==0
    signal(SIGINT, catcher);
    signal(SIGPIPE, catcher1);
#endif
}

bp_long run_service(bp_long sock, char *buf) {
    bp_long ok=0;

    io_trace_sn("Start on fd number", sock);

    ok=sock_read(sock,buf,0);

    strcpy(buf,"yes");
    ok=ok && sock_write(sock,buf,0);

    close_socket(sock);

    return ok;
}


void run_server(bp_long port,bp_long timeout,bp_long fuel) {
    char buffer[MAXBUF];
    bp_long server_sock=new_server(port);
    if(!(server_sock>0)) return;
    while(--fuel) {
       bp_long service_sock=new_service(server_sock,timeout);
       if(service_sock > 0) {
         bp_long ok;
         io_trace_snn("Ready to select, timout (sec), fuel",timeout,fuel);
         ok=run_service(service_sock,buffer);
         if(!ok) io_trace_sn("bad interaction on",service_sock);
       }
    }
    close_socket(server_sock);
    io_trace_sn("Server terminated on port",port);
}

#if VCC==0
bp_long spawn_server(bp_long port) {
  pid_t pid=fork();
  if(pid<0) {
    err_exit(111,"error in fork");
  }
  else if(pid==0) {
    sleep(1);
  }
  else if(pid>0) {
    io_trace_snn("spawned server pid,port",pid,port);
    run_server(port,0,0);
    io_trace_sn("Background server terminated, pid",pid);
  }
  return pid;
}
#endif

void server() {
  init_sigs();
  run_server(SERV_PORT,0,0);
}

bp_long run_client(char *host,bp_long port,char *buf) {
     bp_long s;
     s=new_client(host,port);
     if(s<0) return 0;
     if(!sock_write(s,buf,0)) return 0;
     if(!sock_read(s,buf,0)) return 0;
     close_socket(s);
     return 1;
}

/* simple stubs to try out STADALONE client+server+remote toplevel */

void init_client(char *host) {
  init_sigs();
  strcpy(host,get_host());
  io_trace_ss("host",host);
  io_trace_ss("ip_addr",get_ip_addr());
  io_trace_sn("QLEVEL()",QLEVEL());
}

bp_long client_step(char *host,bp_long *port,char *buffer,bp_long *pid) {
   buffer[0]='\0';
   printf("* <bp_long times> --> repeat out cmd\n");
   printf("@ <no>--> spawns background server on port %d+<no>\n",SERV_PORT);
   printf("& <ip address or server name> --> sets server host\n");
   printf(": <bp_long port> --> sets server port\n");
   while(1) {bp_long l=0;
     io_mes0("?- ");
     if(!gets(buffer)) return 0;
     if(!(l=strlen(buffer))) continue;
     if('*'==*buffer) {
       bp_long i; bp_long max=atoi(buffer+1);
       for(i=0; i<max; i++) {
         sprintf(buffer,"out(a(%d))",i);
         if(!run_client(host,*port,buffer)) {
           io_trace1("client failed");
           return 0;
         }
         io_trace0(".");
       }
       io_trace1("");
     }
#if VCC==0
     else if('@'==*buffer) {
       bp_long k=atoi(buffer+1);
       if(k<0) kill(*pid,SIGKILL);
       else *pid=spawn_server(SERV_PORT+k);
     }
#endif
     else if(':'==*buffer) {
       *port=SERV_PORT+atoi(buffer+1);
     }
     else if('&'==*buffer) {
       strcpy(host,buffer+1);
     }
     else {
       buffer[l]=' ';buffer[l+1]='\0';
       run_client(host,*port,buffer);
     }
     break;
   }
   return 1;
}

void client() {
  bp_long pid=0;
  bp_long port=SERV_PORT;
  char host[256];
  char buffer[MAXBUF];
  init_client(host);
  while(client_step(host,&port,buffer,&pid));
}

/* remote toplevel for BinProlog server */
bp_long rtop(char *passwd) {
  bp_long port=SERV_PORT;
  char host[256];
  char buffer[MAXBUF];
  char line[MAXBUF];

  init_client(host);

  printf("server host: [%s] ",host);
  if(gets(line) && strlen(line)!=0) strcpy(host,line);

  printf("server port: [%d] ",port);
  if(gets(line) && strlen(line)!=0) port=atoi(line);

  printf("server at: %s:%d\n",host,port);

   buffer[0]='\0';
   while(1) {
     bp_long l=0;
	 io_mes0("!- ");
     if(!gets(line)) return 0;
     if(!(l=strlen(line))) continue;
     if(l>0 && '.'==line[l-1])
       line[l-1]='\0';
     else
       line[l]='\0';
     sprintf(buffer,"run(%s,%s,%s). ",passwd,line,line);
     run_client(host,port,buffer);
     fprintf(STD_out,"%s. \n",buffer);
   }
   return 1;
}

/* this ends if defined STANDALONE_SOCKET_STUFF */
#endif
/* this ends if VCC > 0 */
#endif

/*
 * This software is Copyright (c) Barry Wilkes 2001
 * Barry Wilkes grants you the rights to distribute
 * and use this software as governed by the terms
 * of the Lisp Lesser GNU Public License
 * (http://opensource.franz.com/preamble.html),
 * known as the LLGPL.
*/

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <string.h>

/*
 * unix_domain_socket : Returns an open socket on a UNIX pathname source, -1
 * on error.
 */

int
unix_domain_socket (const char* source)
{
  int sockfd;
  struct sockaddr_un servaddr;

  if ((sockfd = socket(AF_UNIX, SOCK_STREAM, 0)) == -1)
    return -1;
  
  memset(&servaddr, 0, sizeof(servaddr));
  servaddr.sun_family = AF_UNIX;
  strncpy(servaddr.sun_path, source, sizeof(servaddr.sun_path) - 1);

  if (connect(sockfd, &servaddr, sizeof(servaddr)))
    return -1;
  return sockfd;

}



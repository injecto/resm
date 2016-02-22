FROM ubuntu:14.04

MAINTAINER Vladimir Glushenkov <injecto.wg@gmail.com>

RUN adduser --system --group resm
WORKDIR /home/resm
COPY resm/ .
RUN chown -R resm:resm .
EXPOSE 8888

USER resm
CMD ["bin/resm", "foreground"]

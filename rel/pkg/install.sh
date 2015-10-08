#!/usr/bin/bash

AWK=/usr/bin/awk
SED=/usr/bin/sed

USER=tachyon
GROUP=$USER

case $2 in
    PRE-INSTALL)
        if grep "^$GROUP:" /etc/group > /dev/null 2>&1
        then
            echo "Group already exists, skipping creation."
        else
            echo Creating tachyon group ...
            groupadd $GROUP
        fi
        if id $USER > /dev/null 2>&1
        then
            echo "User already exists, skipping creation."
        else
            echo Creating tachyon user ...
            useradd -g $GROUP -d /var/db/tachyon -s /bin/false $USER
        fi
        echo Creating directories ...
        mkdir -p /var/db/tachyon
        mkdir -p /var/log/tachyon
        chown -R $USER:$GROUP /var/db/tachyon
        chown -R $USER:$GROUP /var/log/tachyon
        if [ -d /tmp/tachyon ]
        then
            chown -R $USER:$GROUP /tmp/tachyon/
        fi
        ;;
    POST-INSTALL)
        echo Importing service ...
        svccfg import /opt/local/tachyon/share/tachyon.xml
        CONFFILE=/opt/local/tachyon/etc/tachyon.conf
	RULES=/opt/local/tachyon/etc/tachyon.rules
	if [ ! -f "${RULES}" ]
	then
		cp ${RULES}.example ${RULES}
	fi
        if [ ! -f "${CONFFILE}" ]
        then
            echo "Creating new configuration from example file."
            cp ${CONFFILE}.example ${CONFFILE}
        else
            echo "Please make sure you update your config according to the update manual!"
            #/opt/local/fifo-sniffle/share/update_config.sh ${CONFFILE}.example ${CONFFILE} > ${CONFFILE}.new &&
            #    mv ${CONFFILE} ${CONFFILE}.old &&
            #    mv ${CONFFILE}.new ${CONFFILE}
        fi
        ;;
esac

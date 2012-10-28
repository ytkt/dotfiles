# for MacPorts
#export PATH=/opt/local/bin:opt/local/sbin/:$PATH

# php (homebrew)
if [ -f ${HOME}/.bashrc ] ; then
    source ${HOME}/.bashrc
fi


echo "hello"

# ENV settings
if [ -f ${HOME}/.env ] ; then
    source ${HOME}/.env
fi


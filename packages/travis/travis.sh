function linux_before_install()
{
    docker build --tag slimp --file packages/travis/Dockerfile .
}

function linux_script()
{
    umask og+w
    mkdir upload
    docker run -i -t -v$(pwd)/upload:/upload --user=max slimp \
           /bin/bash -c \
           'make -C /home/max/rpmbuild/SOURCES/slimp'
}

${TRAVIS_OS_NAME}_$1

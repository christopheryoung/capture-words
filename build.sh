
#Quick and dirty script for setting up our project

CAPTURE_WORDS_DIR=`dirname "$0"`
PROJECTS_DIR=`( cd .. && pwd )`
SIRCYB_UTILS=$PROJECTS_DIR/sircyb-utils
CHECKOUTS=$CAPTURE_WORDS_DIR/checkouts

#Checkout sircyb-utils if it doesn't already exist

cd $PROJECTS_DIR
if [ ! -d $SIRCYB_UTILS ]; then
    echo "Checking out sircyb-utils"
    git clone git@github.com:christopheryoung/sircyb-utils.git
else
    echo "sircyb-utils already checked out . . . skipping"
fi

#Create sircyb jar and install to local repo
cd $SIRCYB_UTILS
lein jar
cd $CAPTURE_WORDS_DIR
mvn install:install-file -Dfile=checkouts/sircyb-utils/target/sircyb-utils-0.1.0-SNAPSHOT.jar -DcreateChecksum=true -DartifactId=sircyb-utils -Dversion=0.1.0-SNAPSHOT -DgroupId=sircyb-utils -Dpackaging=jar -DlocalRepositoryPath=maven_repo

# Softlink dependencies
cd $CAPTURE_WORDS_DIR
if [ ! -d $CHECKOUTS ]; then
    echo "Creating checkouts directory"
    mkdir $CHECKOUTS
else
    echo "checkouts directory already created . . . skipping"
fi

if [ ! -d $CHECKOUTS/sircyb-utils ]; then
    echo "Linking sircyb-utils"
    ln -s $SIRCYB_UTILS $CHECKOUTS/sircyb-utils
else
    echo "sircyb-utils already linked"
fi

lein deps
lein midje 


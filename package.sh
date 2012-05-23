
cd checkouts/sircyb-utils
lein jar
cd ../..
mvn install:install-file -Dfile=checkouts/sircyb-utils/target/sircyb-utils-0.1.0-SNAPSHOT.jar -DcreateChecksum=true -DartifactId=sircyb-utils -Dversion=0.1.0-SNAPSHOT -DgroupId=sircyb-utils -Dpackaging=jar -DlocalRepositoryPath=maven_repo

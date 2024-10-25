Comment faire le premier test !
Dans le dossier, il y a deux dossiers, gix et me. Gix est un témoin Pour tester, c'est facile, on fait
# Génération
make


# Charge les informations de connexion de la base dans l'environnement
source dbenv.sh


# Lance le programme qui crée la base
./createdb.exe


# Lance le programme qui lit la base
./sumdb.exe




 Il y a peut-être des choses à remplacer dans le makefile, selon l'endroit où vous avez installé les exécutables de gix et superbol


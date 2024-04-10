#####################
# INCC
#####################

# pcneurophys2.biomedicale.univ-paris5.fr or 193.51.83.237
alias pc2='ssh -tXYC alexandre.mahrach@193.51.83.237'
alias tunnelto2='ssh -tL 4022:localhost:4022 alexandre.mahrach@193.51.83.237'

for i in 172 75 76 77 78 79 80 81 82 83 84 85 181 182 183 184; do
    alias pc2to$i="ssh -tXYC alexandre.mahrach@193.51.83.237 'ssh -XYC alexandre.mahrach@172.20.62.$i'"
done

for i in 172 84 85 181 182 183 184; do 
    alias pc2toadm$i="ssh -tXYC alexandre.mahrach@193.51.83.237 'ssh -tXYC leon@172.20.62.$i'"
done

for i in 172 75 76 77 78 79 80 81 82 83 84 85 181 182 183 184; do
    alias tunnelto$i="ssh -tL 4023:localhost:4023 alexandre.mahrach@193.51.83.237 'ssh -tL 4023:localhost:22 alexandre.mahrach@172.20.62.$i'"
done

alias sshfs_paris="sshfs -p 4023 alexandre.mahrach@localhost:/homecentral/alexandre.mahrach/bebopalula ~/paris/"
alias tunneltoParis="tunnelto181"

#####################
# IDIBAPS 
#####################
alias notifyme='ssh leon@172.28.86.215 "notify-send \"Job Done\" \"Your job with id: \$1 has finished\""'

alias ssh_minibaps="ssh -tXYC leon@84.88.67.74 -p 4022 'ssh -tXYC 172.26.20.44' "
alias minibaps="ssh -tXYC 172.26.20.44"

alias ssh_bcn="ssh -XYC leon@neurocomp.fcrb.es -p 4022"
alias tunneltobcn="ssh -L 4022:127.0.0.1:22 leon@neurocomp.fcrb.es -p 4022"

alias rsync_ssh='rsync -ahxv --progress -e "ssh -T -p 4022"'
#user@<source>:<source_dir> <dest_dir>

alias sshfs_bcn="sshfs -p 4022 84.88.67.74:/home/leon/ ~/bcn/"
alias sftp_bcn="sftp -p 4022 84.88.67.74"

#####################
# Remarkable 
#####################

alias ssh_rmk="ssh root@10.204.7.105"
alias ssh_rmk_usb="ssh root@172.19.249.206"

#####################
# HPC
#####################

alias starlife="ssh -tXYC cli29755@sl1.bsc.es"
alias starlife2="ssh -tXYC cli29755@sl2.bsc.es"
# /slgpfs/projects/cli29/

#####################
# gcloud
#####################

alias ulysses="ssh -XYC 35.228.7.31"
alias sshfs_gcloud="sshfs 35.228.7.31:/home/leon ./gcloud"
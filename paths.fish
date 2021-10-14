set dir /usr/share/paths

function paths
    if [ $argv[1] = go ]
        cd ($dir/paths go $argv[2])
    else
        $dir/paths $argv
    end
end

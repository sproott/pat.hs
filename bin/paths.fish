set dir /usr/share/paths

function paths
    if [ $argv[1] = go ]
        set output ($dir/paths go $argv[2])
        if [ $status -ne 0 ]
            echo $output
            return 1
        end
        cd $output
    else
        $dir/paths $argv
    end
end

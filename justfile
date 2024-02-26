_default:
  @just -l -u

# start ghci with the Ghci module loaded
ghci:
  ghci -iapp app/Ghci.hs

# run ormolu and hlint
lint:
  ormolu -i app/*.hs
  hlint app/*.hs

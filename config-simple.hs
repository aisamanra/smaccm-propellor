import Propellor
import Propellor.CmdLine
import qualified Propellor.Property.Apt     as Apt
import qualified Propellor.Property.Cmd     as Cmd
import qualified Propellor.Property.Git     as Git

main :: IO ()
main = do
  defaultMain [host "lemuria", buildHost]

cabalInstall :: String -> Property NoInfo
cabalInstall pkgs = Cmd.cmdProperty "cabal" ["install", pkgs]

allOf :: [Property NoInfo] -> Property NoInfo
allOf xs = simpleProperty "Finished list" (return NoChange) xs

buildHost :: Host
buildHost =
  let usr = "build"
      dir = "/build"
      ivoryRepo = dir <> "/ivory"
      towerRepo = dir <> "/tower"
      bspRepo   = dir <> "/ivory-tower-stm32"
  in host "smaccm-build-comrade.dev.galois.com"
     & ipv4 "192.168.52.236"
     & os (System (Debian (Stable "jessie")) "amd64")
     & Apt.stdSourcesList
     & Apt.update
     & Apt.installed ["ghc","git"]
     & Apt.setSourcesList ["ppa:terry.guo/gcc-arm-embedded"]
     & Apt.installed ["gcc-arm-none-eabi"]
     & Git.cloned usr "https://github.com/galoisinc/ivory" ivoryRepo Nothing
     & Git.cloned usr "https://github.com/galoisinc/tower" towerRepo Nothing
     & Git.cloned usr "https://github.com/galoisinc/ivory-tower-stm32" bspRepo Nothing
     & cabalInstall "cabal-install"
     & cabalInstall "alex"
     & cabalInstall "happy"
     & allOf [ Cmd.cmdProperty' "make" [target]
               [ ("IVORY_REPO", ivoryRepo)
               , ("TOWER_REPO", towerRepo)
               , ("BSP_REPO",   bspRepo)
               ]
             | target <- [ "ivory-px4-hw"
                         , "ivory-px4-tests"
                         , "smaccm-commsec"
                         , "smaccm-mavlink"
                         , "smaccm-datalink"
                         , "smaccm-ins"
                         ]
             ]

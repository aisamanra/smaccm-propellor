import Propellor
import Propellor.CmdLine
import qualified Propellor.Property.Apt     as Apt
import qualified Propellor.Property.Cmd     as Cmd
import qualified Propellor.Property.File    as File
import qualified Propellor.Property.Git     as Git

main :: IO ()
main = do
  defaultMain [buildHost]

cabalInstall :: [String] -> Property NoInfo
cabalInstall pkgs = Cmd.cmdProperty "cabal" ("install":pkgs)

cabalUpdate :: Property NoInfo
cabalUpdate = Cmd.cmdProperty "cabal" ["update"]

allOf :: [Property NoInfo] -> Property NoInfo
allOf xs = simpleProperty "Finished list" (return NoChange) xs

buildHost :: Host
buildHost =
  let usr = "root"
      dir = "/build"
      ivoryRepo = dir <> "/ivory"
      towerRepo = dir <> "/tower"
      bspRepo   = dir <> "/ivory-tower-stm32"
      clone r = Git.cloned usr r dir Nothing
  in host "ec2-52-10-93-116.us-west-2.compute.amazonaws.com"
     & alias "smaccm-build-comrade"
     & os (System (Ubuntu "saucy") "amd64")
     & Apt.installed ["software-properties-common","zlib1g-dev"]
     & Cmd.cmdProperty "add-apt-repository"
         ["-y", "ppa:terry.guo/gcc-arm-embedded"]
     & Apt.update
     & Apt.installed ["gcc-arm-none-eabi"]
     & File.dirExists dir
     & clone "https://github.com/galoisinc/ivory"
     & clone "https://github.com/galoisinc/tower"
     & clone "https://github.com/galoisinc/ivory-tower-stm32"
     & cabalUpdate
     & cabalInstall ["cabal-install", "alex", "happy"]
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

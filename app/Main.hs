import           Web.Spock -- web framework
import           Web.Spock.Config

import           Data.Aeson       hiding (json) -- module to work with json data 
import           Data.Monoid      ((<>))
import           GHC.Generics
import           User            
import           Repository
import           Association

instance ToJSON User.User

instance FromJSON User.User
module Arkham.Location.CardDefs.ReturnToTheForgottenAge.ReturnToShatteredAeons where

import Arkham.Location.CardDefs.Import

buenosAires :: CardDef
buenosAires =
  singleSided
    $ location "53063" "Buenos Aires" [Shattered] Equals [Star] ReturnToShatteredAeons

greatHallOfCeleano :: CardDef
greatHallOfCeleano =
  singleSided
    $ location "53062" "Great Hall of Celeano" [Otherworld] Droplet [Diamond] ReturnToShatteredAeons

ultimaThule :: CardDef
ultimaThule =
  singleSided
    $ location "53064" "Ultima Thule" [Shattered] Equals [Star] ReturnToShatteredAeons

module Arkham.Types.Card.PlayerCard.Cards.SilverTwilightAcolyte where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Trait

newtype SilverTwilightAcolyte = SilverTwilightAcolyte Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env SilverTwilightAcolyte where
  runMessage msg (SilverTwilightAcolyte attrs) =
    SilverTwilightAcolyte <$> runMessage msg attrs

silverTwilightAcolyte :: CardId -> SilverTwilightAcolyte
silverTwilightAcolyte cardId =
  SilverTwilightAcolyte $ (enemy cardId "01102" "Silver Twilight Acolyte" 0)
    { pcTraits = [Humanoid, Cultist, SilverTwilight]
    , pcKeywords = [Keyword.Hunter]
    }

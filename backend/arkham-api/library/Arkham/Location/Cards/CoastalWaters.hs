module Arkham.Location.Cards.CoastalWaters (coastalWaters) where

import Arkham.Ability
import Arkham.Card.CardDef
import Arkham.Helpers.SkillTest
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Stories

newtype CoastalWaters = CoastalWaters LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coastalWaters :: LocationCard CoastalWaters
coastalWaters = location CoastalWaters Cards.coastalWaters 2 (PerPlayer 4)

mirageCards :: [CardDef]
mirageCards = [Cards.airfield, Cards.ottomanFront]

instance HasModifiersFor CoastalWaters where
  getModifiersFor (CoastalWaters a) = clearedOfMirages a mirageCards

instance HasAbilities CoastalWaters where
  getAbilities (CoastalWaters a) =
    extendRevealed
      a
      [ mirage a 2 mirageCards
      , playerLimit PerTestOrAbility
          $ restricted a 1 (DuringSkillTest $ WhileInvestigating (be a))
          $ forced
          $ RevealChaosToken #after You #frost
      ]

instance RunMessage CoastalWaters where
  runMessage msg l@(CoastalWaters attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (DiscoveredClues 1)
      pure l
    _ -> CoastalWaters <$> mirageRunner Stories.coastalWaters mirageCards 2 msg attrs

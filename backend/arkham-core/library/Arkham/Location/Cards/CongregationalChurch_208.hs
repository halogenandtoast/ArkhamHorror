module Arkham.Location.Cards.CongregationalChurch_208
  ( congregationalChurch_208
  , CongregationalChurch_208(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( congregationalChurch_208 )
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Zone

newtype CongregationalChurch_208 = CongregationalChurch_208 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

congregationalChurch_208 :: LocationCard CongregationalChurch_208
congregationalChurch_208 = location
  CongregationalChurch_208
  Cards.congregationalChurch_208
  1
  (PerPlayer 1)

instance HasAbilities CongregationalChurch_208 where
  getAbilities (CongregationalChurch_208 attrs) =
    let rest = withDrawCardUnderneathAction attrs
    in
      rest
        <> [ mkAbility attrs 1
             $ ForcedAbility
             $ RevealLocation Timing.After Anyone
             $ LocationWithId
             $ toId attrs
           | locationRevealed attrs
           ]

instance RunMessage CongregationalChurch_208 where
  runMessage msg l@(CongregationalChurch_208 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push
        $ FindEncounterCard iid (toTarget attrs) [FromEncounterDeck]
        $ CardWithType EnemyType
        <> CardWithTrait Humanoid
      pure l
    FoundEncounterCard _iid target card | isTarget attrs target -> do
      villageCommonsId <- selectJust $ LocationWithTitle "Village Commons"
      l <$ push (SpawnEnemyAt (EncounterCard card) villageCommonsId)
    _ -> CongregationalChurch_208 <$> runMessage msg attrs

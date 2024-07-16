module Arkham.Investigator.Cards.RolandBanks (RolandBanks (..), rolandBanks) where

import Arkham.Discover
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Location.Types
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Projection

newtype RolandBanks = RolandBanks InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock (Data)

rolandBanks :: InvestigatorCard RolandBanks
rolandBanks =
  investigator RolandBanks Cards.rolandBanks
    $ Stats {health = 9, sanity = 5, willpower = 3, intellect = 3, combat = 4, agility = 2}

instance HasAbilities RolandBanks where
  getAbilities (RolandBanks attrs) =
    [ playerLimit PerRound
        $ restrictedAbility attrs 1 (Self <> AbleToDiscoverCluesAt YourLocation)
        $ freeReaction (Matcher.EnemyDefeated #after You ByAny AnyEnemy)
    ]

instance HasChaosTokenValue RolandBanks where
  getChaosTokenValue iid ElderSign (RolandBanks attrs) | attrs `is` iid = do
    mLocation <- field InvestigatorLocation iid
    clues <- maybe (pure 0) (field LocationClues) mLocation
    pure $ ChaosTokenValue ElderSign (PositiveModifier clues)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage RolandBanks where
  runMessage msg i@(RolandBanks attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Msg.DiscoverClues iid $ discoverAtYourLocation (attrs.ability 1) 1
      pure i
    _ -> RolandBanks <$> runMessage msg attrs

module Arkham.Investigator.Cards.TrishScarborough (trishScarborough, TrishScarborough (..)) where

import Arkham.Ability
import Arkham.Discover
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest
import Arkham.Helpers.Window (discoveredLocation)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted hiding (DiscoverClues)
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message.Lifted.Choose
import Arkham.Taboo

newtype TrishScarborough = TrishScarborough InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

trishScarborough :: InvestigatorCard TrishScarborough
trishScarborough =
  investigator TrishScarborough Cards.trishScarborough
    $ Stats {health = 8, sanity = 6, willpower = 2, intellect = 4, combat = 2, agility = 4}

instance HasAbilities TrishScarborough where
  getAbilities (TrishScarborough a) =
    [ playerLimit PerRound
        $ restricted a 1 (Self <> oneOf [exists locationWithAdditionalClues, exists evadableEnemy])
        $ freeReaction
        $ DiscoverClues #after You (LocationWithEnemy AnyEnemy) (atLeast 1)
    ]
   where
    tabooModifier = if tabooed TabooList21 a then (NonEliteEnemy <>) else id
    locationWithAdditionalClues = LocationBeingDiscovered <> LocationWithAnyClues
    evadableEnemy = tabooModifier $ CanEvadeEnemy (toSource a)

instance HasChaosTokenValue TrishScarborough where
  getChaosTokenValue iid ElderSign (TrishScarborough attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage TrishScarborough where
  runMessage msg i@(TrishScarborough attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (discoveredLocation -> lid) _ -> do
      let source = attrs.ability 1
      chooseOrRunOneM iid do
        labeled "Discover 1 additional clue at that location" $ discoverAt NotInvestigate iid source lid 1
        labeled "Automatically evade that enemy" do
          chooseSelectM iid (enemyAt lid <> EnemyCanBeEvadedBy source) $ automaticallyEvadeEnemy iid
      pure i
    ElderSignEffect (is attrs -> True) -> do
      whenM isInvestigation do
        locations <- select . (RevealedLocation <>) . maybe Anywhere (not_ . be) =<< getLocationOf attrs.id
        when (notNull locations) do
          withSkillTest \sid -> chooseOneM attrs.id do
            labeled "Do not choose a different location" nothing
            targets locations \location -> do
              push $ SetSkillTestTarget (toTarget location)
              skillTestModifier sid ElderSign attrs (AsIfAt location)
      pure i
    _ -> TrishScarborough <$> liftRunMessage msg attrs

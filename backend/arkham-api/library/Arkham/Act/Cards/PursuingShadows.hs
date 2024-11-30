module Arkham.Act.Cards.PursuingShadows (PursuingShadows (..), pursuingShadows) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Matcher
import Arkham.Scenarios.APhantomOfTruth.Helpers

newtype PursuingShadows = PursuingShadows ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pursuingShadows :: ActCard PursuingShadows
pursuingShadows = act (2, A) PursuingShadows Cards.pursuingShadows Nothing

instance HasAbilities PursuingShadows where
  getAbilities (PursuingShadows a) | onSide A a = do
    [ reaction a 1 NoRestriction (GroupClueCost (Static 1) YourLocation)
        $ EnemyAttackedSuccessfully #after You AnySource (EnemyWithTitle "The Organist")
      , restricted a 2 (exists $ EnemyWithTitle "The Organist" <> EnemyWithClues (AtLeast $ PerPlayer 3))
          $ Objective
          $ forced AnyWindow
      ]
  getAbilities _ = []

instance RunMessage PursuingShadows where
  runMessage msg a@(PursuingShadows attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      theOrganist <- getTheOrganist
      placeClues (attrs.ability 1) theOrganist 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      intrudedOnASecretMeeting <- getHasRecord YouIntrudedOnASecretMeeting
      push $ if intrudedOnASecretMeeting then R2 else R1
      pure a
    _ -> PursuingShadows <$> liftRunMessage msg attrs

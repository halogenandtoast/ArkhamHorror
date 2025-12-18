module Arkham.Location.Cards.TheKeyReliquarySanctum (theKeyReliquarySanctum) where

import Arkham.Ability
import Arkham.Discover
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Token

newtype TheKeyReliquarySanctum = TheKeyReliquarySanctum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theKeyReliquarySanctum :: LocationCard TheKeyReliquarySanctum
theKeyReliquarySanctum = location TheKeyReliquarySanctum Cards.theKeyReliquarySanctum 5 (PerPlayer 1)

instance HasAbilities TheKeyReliquarySanctum where
  getAbilities (TheKeyReliquarySanctum a) =
    extendRevealed1 a
      $ restricted
        a
        1
        ( Here
            <> exists
              ( ScarletKeyWithInvestigator (InvestigatorAt YourLocation)
                  <> ScarletKeyWithTokens (EqualTo $ Static 0) Empowerment
              )
        )
      $ freeReaction
      $ SkillTestResult #after You (WhileInvestigating $ be a) #success

instance RunMessage TheKeyReliquarySanctum where
  runMessage msg (TheKeyReliquarySanctum attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      pure $ TheKeyReliquarySanctum $ attrs & setMeta True
    Msg.DiscoverClues iid d | d.location == DiscoverAtLocation attrs.id && d.isInvestigate == IsInvestigate -> do
      if getLocationMetaDefault False attrs
        then do
          ks <-
            select
              $ ScarletKeyWithInvestigator (InvestigatorAt YourLocation)
              <> ScarletKeyWithTokens (EqualTo $ Static 0) Empowerment
          chooseTargetM iid ks $ placeTokensOn (attrs.ability 1) Empowerment 1
          pure $ TheKeyReliquarySanctum $ attrs & setMeta False
        else TheKeyReliquarySanctum <$> liftRunMessage msg attrs
    _ -> TheKeyReliquarySanctum <$> liftRunMessage msg attrs

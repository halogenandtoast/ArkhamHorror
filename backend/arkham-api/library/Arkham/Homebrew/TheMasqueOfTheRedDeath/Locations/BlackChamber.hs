module Arkham.Homebrew.TheMasqueOfTheRedDeath.Locations.BlackChamber (blackChamber) where

import Arkham.Ability
import Arkham.ChaosToken (pattern NegativeModifier)
import Arkham.ChaosToken.Types (ChaosTokenValue (ChaosTokenValue))
import Arkham.Helpers.SkillTest (isSkillTestAt, withSkillTest)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (addSkullEffectsToToken, describedSkullEffect)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype BlackChamber = BlackChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackChamber :: LocationCard BlackChamber
blackChamber =
  locationWith BlackChamber Cards.blackChamber 2 (PerPlayer 1)
    $ costToEnterUnrevealedL
    .~ GroupClueCost (PerPlayer 2) Anywhere

instance HasAbilities BlackChamber where
  -- "[skull]: -2. If you fail by 2 or more, choose a non-story asset you control
  -- and return it to your hand."
  getAbilities (BlackChamber a) =
    extendRevealed1 a
      $ describedSkullEffect
        (-2)
        "If you fail by 2 or more, choose a non-story asset you control and return it to your hand."
        a
        1

instance RunMessage BlackChamber where
  runMessage msg l@(BlackChamber attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        skillTestModifier sid (attrs.ability 1) sid
          $ AddChaosTokenValue (ChaosTokenValue #skull (NegativeModifier 2))
        onFailedByEffect sid (atLeast 2) (attrs.ability 1) iid do
          assets <- select $ assetControlledBy iid <> AssetNonStory <> AssetCanLeavePlayByNormalMeans
          chooseTargetM iid assets $ returnToHand iid
      pure l
    -- "Add each [skull] effect on your location to each numeric token you reveal
    -- during skill tests at Black Chamber."
    ResolveChaosToken token face iid
      | attrs.revealed
      , not face.isSymbol -> do
          whenM (isSkillTestAt attrs.id) $ addSkullEffectsToToken iid token
          pure l
    _ -> BlackChamber <$> liftRunMessage msg attrs

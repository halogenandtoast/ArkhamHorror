module Arkham.Homebrew.ThePhantomOfTheOpera.Assets.TheScorpion (theScorpion) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Helpers.Healing (chooseHealDamageOrHorror)
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Assets qualified as Cards
import Arkham.Matcher

newtype TheScorpion = TheScorpion AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theScorpion :: AssetCard TheScorpion
theScorpion = asset TheScorpion Cards.theScorpion

instance HasAbilities TheScorpion where
  getAbilities (TheScorpion a) =
    [ limitedAbility (PlayerLimit PerTest 1)
        $ controlled a 1 (DuringSkillTest $ YourSkillTest #any)
        $ triggered (RevealChaosToken #cancel You (oneOf [#eldersign, #"+1", #"0"])) Free
    , controlled a 2 ControlsThis
        $ triggered (CancelledOrIgnoredCardOrGameEffect (SourceOwnedBy You) Nothing) (exhaust a)
    ]

instance RunMessage TheScorpion where
  runMessage msg a@(TheScorpion attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      cancelChaosToken (attrs.ability 1) iid token
      chooseHealDamageOrHorror (attrs.ability 1) iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 2) 1
      pure a
    _ -> TheScorpion <$> liftRunMessage msg attrs

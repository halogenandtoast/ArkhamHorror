module Arkham.Asset.Assets.WalkInFaithFilledWithSpirit (walkInFaithCompleted) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype WalkInFaithFilledWithSpirit = WalkInFaithFilledWithSpirit AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

walkInFaithCompleted :: AssetCard WalkInFaithFilledWithSpirit
walkInFaithCompleted = asset WalkInFaithFilledWithSpirit Cards.walkInFaithCompleted

instance HasModifiersFor WalkInFaithFilledWithSpirit where
  getModifiersFor (WalkInFaithFilledWithSpirit a) =
    for_ a.controller \iid -> modified_ a iid [SkillModifier #willpower 1]

instance HasAbilities WalkInFaithFilledWithSpirit where
  getAbilities (WalkInFaithFilledWithSpirit a) =
    [ controlled a 1 NoRestriction
        $ freeReaction
        $ RevealChaosToken
          #after
          (InvestigatorAt $ LocationWithInvestigator $ HealableInvestigator (a.ability 1) #horror Anyone)
          #eldersign
    ]

{- | "an investigator at that location" — the window only carries the investigator
who revealed the token, and that investigator's location is the one the token was
revealed at.
-}
revealedBy :: [Window] -> Maybe InvestigatorId
revealedBy ws = listToMaybe [who | (windowType -> Window.RevealChaosToken who _) <- ws]

instance RunMessage WalkInFaithFilledWithSpirit where
  runMessage msg a@(WalkInFaithFilledWithSpirit attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (revealedBy -> Just who) _ -> do
      healable <- select $ HealableInvestigator (attrs.ability 1) #horror (colocatedWith who)
      chooseOrRunOneM iid $ targets healable \i -> healHorror i (attrs.ability 1) 1
      pure a
    _ -> WalkInFaithFilledWithSpirit <$> liftRunMessage msg attrs

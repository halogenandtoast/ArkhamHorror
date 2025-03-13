module Arkham.Asset.Assets.ForbiddenSutra2 (forbiddenSutra2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosBagStepState
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Trait

newtype ForbiddenSutra2 = ForbiddenSutra2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forbiddenSutra2 :: AssetCard ForbiddenSutra2
forbiddenSutra2 = asset ForbiddenSutra2 Cards.forbiddenSutra2

instance HasModifiersFor ForbiddenSutra2 where
  getModifiersFor (ForbiddenSutra2 attrs) = for_ attrs.controller \iid -> do
    resolvingSpell <- selectAny $ ActiveEvent <> withTrait Spell
    modifiedWhen_ attrs resolvingSpell iid [AnySkillValue 1]

instance HasAbilities ForbiddenSutra2 where
  getAbilities (ForbiddenSutra2 x) =
    [ controlled x 1 (DuringSkillTest $ SkillTestOnEvent $ withTrait Spell)
        $ triggered (WouldRevealChaosTokens #when Anyone)
        $ AtLeastOne (Fixed 3) (HorrorCost (x.ability 1) YouTarget 1)
    ]

instance RunMessage ForbiddenSutra2 where
  runMessage msg a@(ForbiddenSutra2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getMaybeDrawSource -> mDrawSource) (horrorPaid -> n) -> do
      case mDrawSource of
        Nothing -> withSkillTest \sid ->
          skillTestModifier sid (attrs.ability 1) sid (ChangeRevealStrategy $ RevealAndChoose (1 + n) 1)
        Just drawSource -> do
          steps <- maybe [] getSteps <$> getChaosBagChoice
          push
            $ ReplaceCurrentDraw drawSource iid
            $ Choose (toSource attrs) 1 ResolveChoice (steps <> replicate n (Undecided Draw)) [] Nothing
          cancelledOrIgnoredCardOrGameEffect attrs
      pure a
    _ -> ForbiddenSutra2 <$> liftRunMessage msg attrs

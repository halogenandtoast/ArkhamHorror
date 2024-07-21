module Arkham.Asset.Cards.PnakoticManuscripts5 (
  pnakoticManuscripts5,
  pnakoticManuscripts5Effect,
  PnakoticManuscripts5 (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype PnakoticManuscripts5 = PnakoticManuscripts5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pnakoticManuscripts5 :: AssetCard PnakoticManuscripts5
pnakoticManuscripts5 = asset PnakoticManuscripts5 Cards.pnakoticManuscripts5

instance HasAbilities PnakoticManuscripts5 where
  getAbilities (PnakoticManuscripts5 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (WouldPerformRevelationSkillTest #when (affectsOthers $ InvestigatorAt YourLocation))
          (assetUseCost a Secret 1)
    , restrictedAbility a 2 ControlsThis
        $ actionAbilityWithCost
        $ assetUseCost a Secret 1
    ]

getInvestigator :: [Window] -> (InvestigatorId, SkillTestId)
getInvestigator [] = error "Invalid call"
getInvestigator ((windowType -> Window.WouldPerformRevelationSkillTest iid sid) : _) =
  (iid, sid)
getInvestigator (_ : xs) = getInvestigator xs

instance RunMessage PnakoticManuscripts5 where
  runMessage msg a@(PnakoticManuscripts5 attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getInvestigator -> (iid, sid)) _ -> do
      push $ skillTestModifier sid (attrs.ability 1) iid DoNotDrawChaosTokensForSkillChecks
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      iids <- select $ affectsOthers $ colocatedWith iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne
          player
          [ targetLabel iid' [createCardEffect Cards.pnakoticManuscripts5 Nothing attrs iid']
          | iid' <- iids
          ]
      pure a
    _ -> PnakoticManuscripts5 <$> runMessage msg attrs

newtype PnakoticManuscripts5Effect = PnakoticManuscripts5Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pnakoticManuscripts5Effect :: EffectArgs -> PnakoticManuscripts5Effect
pnakoticManuscripts5Effect =
  cardEffect PnakoticManuscripts5Effect Cards.pnakoticManuscripts5

instance HasModifiersFor PnakoticManuscripts5Effect where
  getModifiersFor target (PnakoticManuscripts5Effect a) | effectTarget a == target = do
    pure $ toModifiers a [DoNotDrawChaosTokensForSkillChecks]
  getModifiersFor _ _ = pure []

instance RunMessage PnakoticManuscripts5Effect where
  runMessage msg e@(PnakoticManuscripts5Effect attrs@EffectAttrs {..}) =
    case msg of
      SkillTestEnds _ iid _ | InvestigatorTarget iid == effectTarget -> do
        push (DisableEffect effectId)
        pure e
      EndRound -> do
        push (DisableEffect effectId)
        pure e
      _ -> PnakoticManuscripts5Effect <$> runMessage msg attrs

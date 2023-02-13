module Arkham.Asset.Cards.BaronSamedi
  ( baronSamedi
  , BaronSamedi(..)
  )
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Id
import Arkham.Matcher
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window (defaultWindows, Window(..))
import Arkham.Window qualified as Window

newtype BaronSamedi = BaronSamedi AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baronSamedi :: AssetCard BaronSamedi
baronSamedi =
  assetWith BaronSamedi Cards.baronSamedi $ (canLeavePlayByNormalMeansL .~ False)

handleDoom :: Message -> AssetAttrs -> GameT BaronSamedi
handleDoom msg attrs = do
  attrs' <- runMessage msg attrs
  pure $ BaronSamedi $ attrs' { assetCanLeavePlayByNormalMeans = assetDoom attrs' >= 3 }

instance HasAbilities BaronSamedi where
  getAbilities (BaronSamedi a) =
    [ limitedAbility (PlayerLimit PerWindow 1) $ restrictedAbility a 1 ControlsThis
      $ ForcedAbility
      $ PlacedCounter Timing.When 
      (InvestigatorAt $ LocationWithInvestigator $ HasMatchingAsset $ AssetWithId $ toId a) DamageCounter (GreaterThan $ Static 0)
    , restrictedAbility a 2 ControlsThis $ FastAbility $ ExhaustCost $ toTarget a
    ]

toDamagedInvestigator :: [Window] -> InvestigatorId
toDamagedInvestigator [] = error "invalid state"
toDamagedInvestigator (Window _ (Window.PlacedDamage (InvestigatorTarget iid) _) : _) = iid
toDamagedInvestigator (_ : xs) = toDamagedInvestigator xs

instance RunMessage BaronSamedi where
  runMessage msg a@(BaronSamedi attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ PutCardIntoPlay iid (toCard attrs) Nothing (defaultWindows iid)
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 (toDamagedInvestigator -> iid') _ -> do
      push $ InvestigatorDoAssignDamage iid' (toSource attrs) DamageAny (NotAsset AnyAsset)  1 0 [] []
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 ws p -> do
      pushAll [PlaceDoom (toTarget attrs) 1, UseCardAbilityStep iid (toSource attrs) 2 ws p 1]
      pure a
    UseCardAbilityStep _ (isSource attrs -> True) 2 _ _ 1 -> do
      when (assetDoom attrs >= 3) $ push
        $ Discard (toAbilitySource attrs 2) (toTarget attrs)
      pure a
    _ -> handleDoom msg attrs

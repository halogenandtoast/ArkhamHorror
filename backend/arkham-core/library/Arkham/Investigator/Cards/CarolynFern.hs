module Arkham.Investigator.Cards.CarolynFern (
  carolynFern,
  CarolynFern (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Id
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype CarolynFern = CarolynFern InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

carolynFern :: InvestigatorCard CarolynFern
carolynFern =
  investigator CarolynFern Cards.carolynFern
    $ Stats {health = 6, sanity = 9, willpower = 3, intellect = 4, combat = 2, agility = 2}

instance HasAbilities CarolynFern where
  getAbilities (CarolynFern a) =
    [ restrictedAbility
        a
        1
        (Self <> Negate (TreacheryExists $ treacheryIs Treacheries.rationalThought))
        $ freeReaction
        $ OrWindowMatcher
          [ AssetHealed #after #horror (#ally <> AssetControlledBy (affectsOthers Anyone)) (SourceOwnedBy You)
          , InvestigatorHealed #after #horror (affectsOthers Anyone) (SourceOwnedBy You)
          ]
    ]

data HealedThing = HealedAsset AssetId | HealedInvestigator InvestigatorId

getHealed :: [Window] -> HealedThing
getHealed [] = error "invalid call"
getHealed ((windowType -> Window.Healed _ (AssetTarget assetId) _ _) : _) = HealedAsset assetId
getHealed ((windowType -> Window.Healed _ (InvestigatorTarget investigatorId) _ _) : _) =
  HealedInvestigator investigatorId
getHealed (_ : xs) = getHealed xs

instance HasChaosTokenValue CarolynFern where
  getChaosTokenValue iid ElderSign (CarolynFern attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign NoModifier
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage CarolynFern where
  runMessage msg i@(CarolynFern attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getHealed -> healedThing) _ -> do
      healedController <- case healedThing of
        HealedInvestigator iid' -> pure iid'
        HealedAsset aid -> fieldJust AssetController aid
      canGainResources <- selectNone $ treacheryIs Treacheries.rationalThought
      pushWhen (healedController /= toId attrs || canGainResources)
        $ TakeResources healedController 1 (toAbilitySource attrs 1) False
      pure i
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      investigatorsWithHealHorror <- getInvestigatorsWithHealHorror attrs 1 $ colocatedWith iid
      assetsWithHorror <- selectTargets $ HealableAsset (toSource attrs) #horror (assetAtLocationWith iid)
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ Label "Do not heal anything" []
        : targetLabels assetsWithHorror (\target -> only $ HealHorror target (toSource ElderSign) 1)
          <> map (uncurry targetLabel . second only) investigatorsWithHealHorror
      pure i
    _ -> CarolynFern <$> runMessage msg attrs

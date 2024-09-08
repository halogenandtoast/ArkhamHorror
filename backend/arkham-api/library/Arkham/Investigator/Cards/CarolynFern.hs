module Arkham.Investigator.Cards.CarolynFern (carolynFern, CarolynFern (..)) where

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Id
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype CarolynFern = CarolynFern InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

carolynFern :: InvestigatorCard CarolynFern
carolynFern =
  investigator CarolynFern Cards.carolynFern
    $ Stats {health = 6, sanity = 9, willpower = 3, intellect = 4, combat = 2, agility = 2}

instance HasAbilities CarolynFern where
  getAbilities (CarolynFern a) =
    [ restrictedAbility a 1 (Self <> not_ (exists $ treacheryIs Treacheries.rationalThought))
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
  getChaosTokenValue iid ElderSign (CarolynFern attrs) | iid == attrs.id = do
    pure $ ChaosTokenValue ElderSign NoModifier
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage CarolynFern where
  runMessage msg i@(CarolynFern attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getHealed -> healedThing) _ -> do
      healedController <- case healedThing of
        HealedInvestigator iid' -> pure iid'
        HealedAsset aid -> fieldJust AssetController aid
      canGainResources <- selectNone $ treacheryIs Treacheries.rationalThought
      when (healedController /= attrs.id || canGainResources) do
        gainResourcesIfCan healedController (attrs.ability 1) 1
      pure i
    ElderSignEffect iid | iid == attrs.id -> do
      investigators <- selectTargets $ HealableInvestigator (toSource attrs) #horror $ colocatedWith iid
      assetsWithHorror <- selectTargets $ HealableAsset (toSource attrs) #horror (assetAtLocationWith iid)
      chooseOrRunOne iid
        $ Label "Do not heal anything" []
        : targetLabels
          (assetsWithHorror <> investigators)
          (\target -> only $ HealHorror target (ElderSignEffectSource iid) 1)
      pure i
    _ -> CarolynFern <$> liftRunMessage msg attrs

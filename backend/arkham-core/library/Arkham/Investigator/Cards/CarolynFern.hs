module Arkham.Investigator.Cards.CarolynFern
  ( carolynFern
  , CarolynFern(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Types ( Field (..) )
import Arkham.Cost
import Arkham.Criteria
import Arkham.Damage
import Arkham.Id
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype CarolynFern = CarolynFern InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

carolynFern :: InvestigatorCard CarolynFern
carolynFern = investigator
  CarolynFern
  Cards.carolynFern
  Stats
    { health = 6
    , sanity = 9
    , willpower = 3
    , intellect = 4
    , combat = 2
    , agility = 2
    }

instance HasAbilities CarolynFern where
  getAbilities (CarolynFern a) =
    [ restrictedAbility a 1 Self $ ReactionAbility
        (OrWindowMatcher
          [ AssetHealed
            Timing.After
            HorrorType
            (AllyAsset <> AssetControlledBy Anyone)
            (SourceOwnedBy You)
          , InvestigatorHealed
            Timing.After
            HorrorType
            Anyone
            (SourceOwnedBy You)
          ]
        )
        Free
    ]

data HealedThing = HealedAsset AssetId | HealedInvestigator InvestigatorId

getHealed :: [Window] -> HealedThing
getHealed [] = error "invalid call"
getHealed (Window _ (Window.Healed _ (AssetTarget assetId) _ _) : _) =
  HealedAsset assetId
getHealed (Window _ (Window.Healed _ (InvestigatorTarget investigatorId) _ _) : _)
  = HealedInvestigator investigatorId
getHealed (_ : xs) = getHealed xs

instance HasTokenValue CarolynFern where
  getTokenValue iid ElderSign (CarolynFern attrs) | iid == toId attrs = do
    pure $ TokenValue ElderSign NoModifier
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance RunMessage CarolynFern where
  runMessage msg i@(CarolynFern attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getHealed -> healedThing) _ ->
      do
        healedController <- case healedThing of
          HealedInvestigator iid' -> pure iid'
          HealedAsset aid ->
            fieldMap AssetController (fromJustNote "must be controlled") aid
        push $ TakeResources healedController 1 False
        pure i
    ResolveToken _drawnToken ElderSign iid | iid == toId attrs -> do
      investigatorsWithHorror <-
        selectListMap InvestigatorTarget
        $ colocatedWith iid
        <> InvestigatorWithAnyHorror
      assetsWithHorror <-
        selectListMap AssetTarget
        $ AssetAt (locationWithInvestigator iid)
        <> AssetWithHorror
      push
        $ chooseOrRunOne iid
        $ Label "Do not heal anything" []
        : [ TargetLabel target [HealHorror target (toSource attrs) 1]
          | target <- investigatorsWithHorror <> assetsWithHorror
          ]
      pure i
    _ -> CarolynFern <$> runMessage msg attrs

module Arkham.Investigator.Cards.PrestonFairmont (
  prestonFairmont,
  PrestonFairmont (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Game.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection

newtype PrestonFairmont = PrestonFairmont InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prestonFairmont :: InvestigatorCard PrestonFairmont
prestonFairmont =
  investigator
    PrestonFairmont
    Cards.prestonFairmont
    Stats
      { health = 7
      , sanity = 7
      , willpower = 1
      , intellect = 1
      , combat = 1
      , agility = 1
      }

instance HasTokenValue PrestonFairmont where
  getTokenValue iid ElderSign (PrestonFairmont attrs) | iid == toId attrs = do
    pure $ TokenValue ElderSign $ PositiveModifier 0
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance RunMessage PrestonFairmont where
  runMessage msg i@(PrestonFairmont attrs) = case msg of
    TakeResources iid n source False | iid == toId attrs -> do
      case source of
        InvestigatorSource iid'
          | iid == iid' ->
              PrestonFairmont <$> runMessage msg attrs
        AbilitySource abilitySource _ -> do
          familyInheritance <- selectJust $ assetIs Assets.familyInheritance
          if abilitySource == AssetSource familyInheritance
            then PrestonFairmont <$> runMessage msg attrs
            else do
              cannotGainResources <- hasModifier attrs CannotGainResources
              unless cannotGainResources $ do
                push $ PlaceResources (toSource attrs) (AssetTarget familyInheritance) n
              pure i
        _ -> do
          familyInheritance <- selectJust $ assetIs Assets.familyInheritance
          cannotGainResources <- hasModifier attrs CannotGainResources
          unless cannotGainResources $ do
            push $ PlaceResources (toSource attrs) (AssetTarget familyInheritance) n
          pure i
    ResolveToken _drawnToken ElderSign iid | iid == toId attrs -> do
      hasResources <- (> 0) <$> getSpendableResources iid
      push
        $ chooseOrRunOne
          iid
        $ Label "Resolve normally" []
          : [Label "Automatically succeed" [SpendResources iid 2, PassSkillTest] | hasResources]
      pure i
    SpendResources iid n | iid == toId attrs -> do
      familyInheritance <- selectJust $ assetIs Assets.familyInheritance
      familyInheritanceResources <- field AssetResources familyInheritance
      if familyInheritanceResources > 0
        then do
          if investigatorResources attrs > 0
            then
              push $
                chooseOrRunN iid n $
                  replicate
                    familyInheritanceResources
                    ( targetLabel
                        familyInheritance
                        [RemoveResources (toSource attrs) (AssetTarget familyInheritance) 1]
                    )
                    <> replicate
                      (investigatorResources attrs)
                      (ComponentLabel (InvestigatorComponent iid ResourceToken) [Do (SpendResources iid 1)])
            else push $ RemoveResources (toSource attrs) (AssetTarget familyInheritance) n
          pure i
        else PrestonFairmont <$> runMessage msg attrs
    _ -> PrestonFairmont <$> runMessage msg attrs

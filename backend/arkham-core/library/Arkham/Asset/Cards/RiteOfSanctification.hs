module Arkham.Asset.Cards.RiteOfSanctification (
  riteOfSanctification,
  RiteOfSanctification (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (PlayCard)
import Arkham.Card
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype RiteOfSanctification = RiteOfSanctification AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfSanctification :: AssetCard RiteOfSanctification
riteOfSanctification =
  assetWith RiteOfSanctification Cards.riteOfSanctification (setMeta @Bool False)

instance HasModifiersFor RiteOfSanctification where
  getModifiersFor (InvestigatorTarget iid) (RiteOfSanctification a) | not (assetExhausted a) = do
    sameLocation <- onSameLocation iid a.placement
    -- investigator attrs your location
    pure $ toModifiers a [CanReduceCostOf AnyCard 2 | sameLocation]
  getModifiersFor _ _ = pure []

instance HasAbilities RiteOfSanctification where
  getAbilities (RiteOfSanctification attrs) =
    let active = toResult @Bool attrs.meta
     in restrictedAbility
          attrs
          1
          ControlsThis
          ( ReactionAbility
              (PlayCard #when (InvestigatorAt YourLocation) #any)
              (exhaust attrs <> ReleaseChaosTokensCost 1 #any)
          )
          : [ restrictedAbility attrs 2 (exists $ be attrs <> AssetWithoutSealedTokens)
              $ SilentForcedAbility AnyWindow
            | active
            ]

getDetails :: [Window] -> (InvestigatorId, Card)
getDetails [] = error "Wrong window"
getDetails ((windowType -> Window.PlayCard iid card) : _) = (iid, card)
getDetails (_ : xs) = getDetails xs

instance RunMessage RiteOfSanctification where
  runMessage msg a@(RiteOfSanctification attrs) = case msg of
    ResolvedCard _ card | toCardId card == toCardId attrs -> do
      RiteOfSanctification <$> runMessage msg (setMeta True attrs)
    UseCardAbility _iid (isSource attrs -> True) 1 (getDetails -> (current, card)) _ -> do
      push
        $ CreateWindowModifierEffect
          EffectCostWindow
          ( EffectModifiers
              $ toModifiers attrs [ReduceCostOf (CardWithId $ toCardId card) 2]
          )
          (toSource attrs)
          (InvestigatorTarget current)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ toDiscard (attrs.ability 2) attrs
      pure a
    _ -> RiteOfSanctification <$> runMessage msg attrs

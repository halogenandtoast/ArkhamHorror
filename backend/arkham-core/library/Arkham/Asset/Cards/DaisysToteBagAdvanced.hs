module Arkham.Asset.Cards.DaisysToteBagAdvanced
  ( daisysToteBagAdvanced
  , DaisysToteBagAdvanced(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Matcher qualified as Matcher
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

newtype DaisysToteBagAdvanced = DaisysToteBagAdvanced AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daisysToteBagAdvanced :: AssetCard DaisysToteBagAdvanced
daisysToteBagAdvanced = asset DaisysToteBagAdvanced Cards.daisysToteBagAdvanced

instance HasAbilities DaisysToteBagAdvanced where
  getAbilities (DaisysToteBagAdvanced a) =
    [ restrictedAbility a 1 (ControlsThis <> DuringTurn You)
        $ ReactionAbility
            (Matcher.PlayCard
              Timing.When
              You
              (BasicCardMatch $ CardWithTrait Tome)
            )
        $ ExhaustCost (toTarget a)
    ]

instance HasModifiersFor DaisysToteBagAdvanced where
  getModifiersFor (InvestigatorTarget iid) (DaisysToteBagAdvanced a)
    | controlledBy a iid = pure
      [toModifier a $ CanBecomeFast $ CardWithType AssetType <> CardWithTrait Tome]
  getModifiersFor _ _ = pure []

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Tome Nothing

instance RunMessage DaisysToteBagAdvanced where
  runMessage msg a@(DaisysToteBagAdvanced attrs) = case msg of
    -- Slots need to be added before the asset is played so we hook into played card
    PlayedCard iid card | toCardId card == toCardId attrs -> do
      pushAll $ replicate 2 (AddSlot iid HandSlot (slot attrs))
      DaisysToteBagAdvanced <$> runMessage msg attrs
    UseCardAbility _ source 1 [Window Timing.When (Window.PlayCard _ card)] _
      | isSource attrs source
      -> a <$ push
        (CreateEffect "90002" Nothing source (CardIdTarget $ toCardId card))
    _ -> DaisysToteBagAdvanced <$> runMessage msg attrs

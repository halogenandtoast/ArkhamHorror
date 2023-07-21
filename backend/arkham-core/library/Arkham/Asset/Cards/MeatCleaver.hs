module Arkham.Asset.Cards.MeatCleaver (
  meatCleaver,
  MeatCleaver (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card.CardCode
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.SkillType

newtype MeatCleaver = MeatCleaver AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

meatCleaver :: AssetCard MeatCleaver
meatCleaver = asset MeatCleaver Cards.meatCleaver

instance HasAbilities MeatCleaver where
  getAbilities (MeatCleaver attrs) =
    [ restrictedAbility attrs 1 ControlsThis $
        ActionAbility (Just Action.Fight) $
          Costs [ActionCost 1, UpTo 1 (HorrorCost (toSource attrs) YouTarget 1)]
    ]

paidHorror :: Payment -> Bool
paidHorror (HorrorPayment _) = True
paidHorror (Payments ps) = any paidHorror ps
paidHorror _ = False

instance RunMessage MeatCleaver where
  runMessage msg a@(MeatCleaver attrs) = case msg of
    UseCardAbility iid source 1 _ payments | isSource attrs source -> do
      remainingSanity <- field InvestigatorRemainingSanity iid
      a
        <$ pushAll
          [ skillTestModifiers
              attrs
              (InvestigatorTarget iid)
              ( [SkillModifier SkillCombat (if remainingSanity <= 3 then 2 else 1)]
                  <> [DamageDealt 1 | paidHorror payments]
              )
          , CreateEffect
              (toCardCode attrs)
              Nothing
              source
              (InvestigatorTarget iid)
          , ChooseFightEnemy iid source Nothing SkillCombat mempty False
          ]
    _ -> MeatCleaver <$> runMessage msg attrs

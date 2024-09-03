module Arkham.Asset.Cards.PurifyingCorruption4 (
  purifyingCorruption4,
  PurifyingCorruption4 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed)
import Arkham.Matcher
import Arkham.Message (MessageType (..))
import Arkham.Token

newtype PurifyingCorruption4 = PurifyingCorruption4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

purifyingCorruption4 :: AssetCard PurifyingCorruption4
purifyingCorruption4 = asset PurifyingCorruption4 Cards.purifyingCorruption4

instance HasAbilities PurifyingCorruption4 where
  getAbilities (PurifyingCorruption4 x) =
    [ restrictedAbility x 1 ControlsThis
        $ ReactionAbility
          (DrawCard #when You (basic $ NonWeakness <> #treachery) AnyDeck)
          (HorrorCost (x.ability 1) YouTarget 1 <> DamageCost (x.ability 1) YouTarget 1)
    , controlledAbility
        x
        2
        ( oneOf
            [ exists
                $ oneOf [HealableInvestigator (x.ability 2) damageType You | damageType <- [#horror, #damage]]
            , exists $ be x <> AssetWithTokens (atLeast 1) Corruption
            ]
        )
        $ FastAbility (DrawEncounterCardsCost 1)
    ]

instance RunMessage PurifyingCorruption4 where
  runMessage msg a@(PurifyingCorruption4 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      pushAll
        [ CancelEachNext (toSource attrs) [RevelationMessage, DrawEnemyMessage]
        , PlaceTokens (attrs.ability 1) (toTarget attrs) Corruption 1
        ]
      pure a
    PlaceTokens _ (isTarget attrs -> True) Corruption n -> do
      when (attrs.token Corruption + n >= 3) $ do
        push $ RemoveFromGame (toTarget attrs)
      PurifyingCorruption4 <$> liftRunMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      canHealDamage <- canHaveDamageHealed (attrs.ability 2) iid
      canHealHorror <- canHaveHorrorHealed (attrs.ability 2) iid
      chooseOrRunOne iid
        $ [ Label "Heal 1 damage and 1 horror"
            $ [HealDamage (toTarget iid) (attrs.ability 2) 1 | canHealDamage]
            <> [HealHorror (toTarget iid) (attrs.ability 2) 1 | canHealHorror]
          | canHealDamage || canHealHorror
          ]
        <> [ Label
            "Remove 1 corruption from this card"
            [RemoveTokens (attrs.ability 2) (toTarget attrs) Corruption 1]
           | attrs.token Corruption > 0
           ]
      pure a
    _ -> PurifyingCorruption4 <$> liftRunMessage msg attrs

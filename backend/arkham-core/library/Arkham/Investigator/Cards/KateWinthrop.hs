module Arkham.Investigator.Cards.KateWinthrop (kateWinthrop, KateWinthrop (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Token

newtype KateWinthrop = KateWinthrop InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

kateWinthrop :: InvestigatorCard KateWinthrop
kateWinthrop =
  startsWith [Assets.fluxStabilizerInactive]
    $ investigator KateWinthrop Cards.kateWinthrop
    $ Stats {health = 6, sanity = 8, willpower = 2, intellect = 4, combat = 2, agility = 4}

instance HasAbilities KateWinthrop where
  getAbilities (KateWinthrop a) =
    [ restrictedAbility
        a
        1
        ( Self
            <> youExist InvestigatorWithAnyClues
            <> exists (oneOf [#science, #tool] <> AssetControlledBy You <> not_ AssetWithAnyClues)
        )
        $ FastAbility Free
    , restrictedAbility a 2 (Self <> youExist (InvestigatorAt Anywhere))
        $ forced
        $ AssetLeavesPlay #when
        $ AssetControlledBy You
        <> AssetWithAnyClues
    ]

instance HasChaosTokenValue KateWinthrop where
  getChaosTokenValue iid ElderSign (KateWinthrop attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 0)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage KateWinthrop where
  runMessage msg i@(KateWinthrop attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOneToHandle iid (attrs.ability 1)
        $ oneOf [#science, #tool]
        <> assetControlledBy iid
        <> not_ AssetWithAnyClues
      pure i
    HandleTargetChoice _iid (isAbilitySource attrs 1 -> True) (AssetTarget aid) -> do
      push $ MoveTokens (attrs.ability 1) (toSource attrs) (toTarget aid) Clue 1
      pure i
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withLocationOf iid \lid -> do
        placeClues (attrs.ability 2) lid 1
      pure i
    ElderSignEffect iid | iid == toId attrs -> do
      assets <- select $ assetControlledBy iid <> AssetWithAnyClues
      when (notNull assets) do
        chooseOne iid $ Label "Do not move clues back" []
          : [ targetLabel asset [MoveTokens (attrs.ability 2) (toSource asset) (toTarget attrs) Clue 1]
            | asset <- assets
            ]
      pure i
    _ -> KateWinthrop <$> liftRunMessage msg attrs

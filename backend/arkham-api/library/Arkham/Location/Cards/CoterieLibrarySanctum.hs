module Arkham.Location.Cards.CoterieLibrarySanctum (coterieLibrarySanctum, coterieLibrarySanctumEffect) where

import Arkham.Ability
import Arkham.Card
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Conspirator))

newtype CoterieLibrarySanctum = CoterieLibrarySanctum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coterieLibrarySanctum :: LocationCard CoterieLibrarySanctum
coterieLibrarySanctum = location CoterieLibrarySanctum Cards.coterieLibrarySanctum 4 (PerPlayer 1)

instance HasAbilities CoterieLibrarySanctum where
  getAbilities (CoterieLibrarySanctum a) =
    extendRevealed
      a
      [ restricted a 1 Here actionAbility
      , restricted a 2 Here $ FastAbility (ExhaustAssetCost $ AssetWithTrait Conspirator)
      ]

instance RunMessage CoterieLibrarySanctum where
  runMessage msg l@(CoterieLibrarySanctum attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      createCardEffect Cards.coterieLibrarySanctum Nothing (attrs.ability 1) iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      createCardEffect Cards.coterieLibrarySanctum Nothing (attrs.ability 2) iid
      pure l
    _ -> CoterieLibrarySanctum <$> liftRunMessage msg attrs

newtype CoterieLibrarySanctumEffect = CoterieLibrarySanctumEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coterieLibrarySanctumEffect :: EffectArgs -> CoterieLibrarySanctumEffect
coterieLibrarySanctumEffect = cardEffect CoterieLibrarySanctumEffect Cards.coterieLibrarySanctum

instance HasModifiersFor CoterieLibrarySanctumEffect where
  getModifiersFor (CoterieLibrarySanctumEffect a) = for_ a.target.investigator \iid ->
    modified_ a iid [ReduceCostOf #asset 2]

instance RunMessage CoterieLibrarySanctumEffect where
  runMessage msg e@(CoterieLibrarySanctumEffect attrs) = runQueueT $ case msg of
    CardEnteredPlay iid card -> do
      when (attrs.target.investigator == Just iid && cardMatch card (#asset :: CardMatcher)) $ disable attrs
      pure e
    EndRound -> disableReturn e
    _ -> CoterieLibrarySanctumEffect <$> liftRunMessage msg attrs

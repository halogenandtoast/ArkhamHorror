module Arkham.Asset.Assets.TheBlackBook (theBlackBook, TheBlackBook (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (PlayCard)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.Window (cardPlayed)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType ()

newtype TheBlackBook = TheBlackBook AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBlackBook :: AssetCard TheBlackBook
theBlackBook = asset TheBlackBook Cards.theBlackBook

instance HasModifiersFor TheBlackBook where
  getModifiersFor (TheBlackBook a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> do
      sanity <- field InvestigatorRemainingSanity iid
      modified_
        a
        iid
        [ SkillModifier #willpower 1
        , SkillModifier #intellect 1
        , CanReduceCostOf AnyCard sanity
        ]

instance HasAbilities TheBlackBook where
  getAbilities (TheBlackBook a) =
    [ restricted a 1 ControlsThis
        $ ReactionAbility (PlayCard #when You #any) (exhaust a <> HorrorCostX (toSource a))
    ]

toHorror :: Payment -> Int
toHorror = \case
  HorrorPayment n -> n
  Payments ps -> sum $ map toHorror ps
  _ -> 0

instance RunMessage TheBlackBook where
  runMessage msg a@(TheBlackBook attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (cardPlayed -> card) (toHorror -> n) -> do
      reduceCostOf attrs card n
      pure a
    _ -> TheBlackBook <$> liftRunMessage msg attrs

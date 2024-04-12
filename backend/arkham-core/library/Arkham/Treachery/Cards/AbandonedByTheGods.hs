module Arkham.Treachery.Cards.AbandonedByTheGods (
  abandonedByTheGods,
  AbandonedByTheGods (..),
)
where

import Arkham.Helpers.Message.Discard
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AbandonedByTheGods = AbandonedByTheGods TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abandonedByTheGods :: TreacheryCard AbandonedByTheGods
abandonedByTheGods = treacheryWith AbandonedByTheGods Cards.abandonedByTheGods (setMeta @[Int] [])

instance RunMessage AbandonedByTheGods where
  runMessage msg t@(AbandonedByTheGods attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      revelationSkillTest iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) (min 5 -> n) -> do
      chooseN
        iid
        n
        [ Label ("Choose " <> tshow x) [HandleAbilityOption iid (toSource attrs) x]
        | x <- [0 .. 4]
        ]
      push $ DoStep 1 msg
      pure t
    HandleAbilityOption _ (isSource attrs -> True) n -> do
      let xs = toResult @[Int] attrs.meta
      pure $ AbandonedByTheGods $ setMeta (n : xs) attrs
    DoStep 1 (FailedThisSkillTestBy _ (isSource attrs -> True) _) -> do
      let xs = toResult @[Int] attrs.meta
      eachInvestigator \iid -> do
        push $ discardAll iid attrs (oneOf [#event, #asset] <> oneOf (map CardWithCost xs))
      pure t
    _ -> AbandonedByTheGods <$> lift (runMessage msg attrs)

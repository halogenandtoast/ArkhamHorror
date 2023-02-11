module Arkham.Treachery.Cards.LostHumanity
  ( lostHumanity
  , LostHumanity(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Helpers
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Message
import Arkham.Projection
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype LostHumanity = LostHumanity TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostHumanity :: TreacheryCard LostHumanity
lostHumanity = treachery LostHumanity Cards.lostHumanity

instance RunMessage LostHumanity where
  runMessage msg t@(LostHumanity attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      pushAll
        [ RevelationSkillTest iid (toSource attrs) SkillWillpower 5
        , RevelationChoice iid (toSource attrs) 1
        ]
      pure t
    RevelationChoice iid (isSource attrs -> True) 1 -> do
      handCount <- fieldMap InvestigatorHand length iid
      deckCount <- fieldMap InvestigatorDeck (length . unDeck) iid
      discardCount <- fieldMap InvestigatorDiscard length iid
      when (handCount + deckCount + discardCount < 10) $ push $ DrivenInsane iid
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ n
      -> do
        push $ DiscardTopOfDeck iid n (toSource attrs) Nothing
        pure t
    _ -> LostHumanity <$> runMessage msg attrs

module Arkham.Location.Cards.AbbeyChurch (abbeyChurch, AbbeyChurch (..)) where

import Arkham.Ability
import Arkham.Agenda.Sequence qualified as AS
import Arkham.Agenda.Types (Field (AgendaSequence))
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype AbbeyChurch = AbbeyChurch LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abbeyChurch :: LocationCard AbbeyChurch
abbeyChurch =
  locationWith AbbeyChurch Cards.abbeyChurch 3 (PerPlayer 1)
    $ costToEnterUnrevealedL
    .~ GroupClueCost (PerPlayer 3) "Broken Steps"

anyDifferent :: Eq a => [a] -> Bool
anyDifferent [] = False
anyDifferent [_] = False
anyDifferent (x : rest@(y : _)) = if x /= y then True else anyDifferent rest

instance HasModifiersFor AbbeyChurch where
  getModifiersFor (AbbeyChurch a) = whenRevealed a $ maybeModifySelf a do
    as <- map AS.agendaStep <$> selectAgg pure AgendaSequence AnyAgenda
    guard $ anyDifferent as
    pure [ShroudModifier 2]

instance HasAbilities AbbeyChurch where
  getAbilities (AbbeyChurch a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)

instance RunMessage AbbeyChurch where
  runMessage msg l@(AbbeyChurch attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      let titles = ["Chœur Gothique", "Knight's Hall", "Cloister", "Chapel of St. Aubert", "Abbey Tower"]
      pushAll $ map (PlaceLocationMatching . CardWithTitle) titles
      pure l
    _ -> AbbeyChurch <$> liftRunMessage msg attrs

module Arkham.Location.Cards.UniversityHalls (universityHalls) where

import Arkham.Ability
import Arkham.Card
import Arkham.Constants
import Arkham.EncounterSet (EncounterSet (Tekelili))
import Arkham.Id
import Arkham.Location.Base (getLocationMeta)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Stories

newtype UniversityHalls = UniversityHalls LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

universityHalls :: LocationCard UniversityHalls
universityHalls = location UniversityHalls Cards.universityHalls 2 (PerPlayer 3)

mirageCards :: [CardDef]
mirageCards = [Cards.elderChamber, Cards.riverviewTheatre, Cards.standingStones]

instance HasModifiersFor UniversityHalls where
  getModifiersFor (UniversityHalls a) = clearedOfMirages a mirageCards

instance HasAbilities UniversityHalls where
  getAbilities (UniversityHalls a) =
    extendRevealed
      a
      [ mirage a 1 mirageCards
      , mkAbility a 1 $ forced $ FlipLocation #when Anyone (be a)
      ]

instance RunMessage UniversityHalls where
  runMessage msg l@(UniversityHalls attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) MirageAbility _ (totalCluePaymentPerInvestigator -> p) -> do
      -- can't just use the mirage runner because we need to set the total clue payment
      attrs' <- mirageRunner Stories.universityHalls mirageCards 1 msg attrs
      pure $ UniversityHalls $ attrs' & setMeta p
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      case getLocationMeta @[(InvestigatorId, Int)] attrs of
        Nothing -> error "Unexpected"
        Just payments -> for_ payments \(iid, n) ->
          discardTopOfDeckAndHandle iid (attrs.ability 2) (n * 3) attrs
      pure l
    DiscardedTopOfDeck iid cards (isAbilitySource attrs 2 -> True) (isTarget attrs -> True) -> do
      let tekelili = filterCards (CardFromEncounterSet Tekelili) cards
      focusCards_ tekelili $ chooseOrRunOneM iid $ targets tekelili $ drawCard iid
      pure l
    _ -> UniversityHalls <$> mirageRunner Stories.universityHalls mirageCards 1 msg attrs

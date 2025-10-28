module Arkham.Act.Cards.FindAmaranth (findAmaranth) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as ScarletKeys
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted.Story
import Arkham.Placement
import Arkham.Story.Cards qualified as Stories
import Arkham.Trait (Trait (RitualSite))

newtype FindAmaranth = FindAmaranth ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

findAmaranth :: ActCard FindAmaranth
findAmaranth = act (1, A) FindAmaranth Cards.findAmaranth Nothing

instance HasAbilities FindAmaranth where
  getAbilities (FindAmaranth x) =
    [ mkAbility x 1
        $ Objective
        $ triggered (RoundEnds #when)
        $ GroupClueCost (PerPlayer 2) (LocationWithTrait RitualSite)
    ]

instance RunMessage FindAmaranth where
  runMessage msg a@(FindAmaranth attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      advanceVia #clues attrs (attrs.ability 1)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      amaranthCard <- fetchCard Enemies.amaranthLurkingCorruption
      bahiaPalaceGardens <- selectJust $ location_ "Bahia Palace Gardens"
      amaranth <- createEnemy amaranthCard bahiaPalaceGardens
      createScarletKeyAt_ ScarletKeys.theLastBlossom (AttachedToEnemy amaranth)
      saveTheCivilians <- fetchCard Stories.saveTheCivilians
      lead <- getLead
      resolveStoryWithPlacement lead saveTheCivilians Global
      khalid <- fetchCard Enemies.khalidBelovedCompanion
      shuffleCardsIntoDeck Deck.EncounterDeck [khalid]
      advanceActDeck attrs
      pure a
    _ -> FindAmaranth <$> liftRunMessage msg attrs

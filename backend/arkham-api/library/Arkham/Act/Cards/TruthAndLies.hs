module Arkham.Act.Cards.TruthAndLies (truthAndLies) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheDreamEaters.Key
import Arkham.Card
import Arkham.Enemy
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (EnemyAttrs (..), Field (..))
import Arkham.Field
import Arkham.Helpers.Calculation
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Modifier (ModifierType (AddKeyword, GainVictory))
import Arkham.Placement
import Arkham.Strategy

newtype TruthAndLies = TruthAndLies ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

truthAndLies :: ActCard TruthAndLies
truthAndLies = act (4, A) TruthAndLies Cards.truthAndLies Nothing

instance HasAbilities TruthAndLies where
  getAbilities (TruthAndLies attrs) =
    [ mkAbility attrs 1 $ actionAbilityWithCost (ClueCost $ Static 1)
    , restricted attrs 2 (InVictoryDisplay "Nyarlathotep" (EqualTo $ StaticWithPerPlayer 1 1))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage TruthAndLies where
  runMessage msg a@(TruthAndLies attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      knowsTheTruth <- getHasRecord TheBlackCatKnowsTheTruth
      possessTheSilverKey <- getHasRecord TheInvestigatorsPossessTheSilverKey

      if knowsTheTruth && possessTheSilverKey
        then do
          nyarlathoteps <- selectForEach (VictoryDisplayCardMatch $ basic $ CardWithTitle "Nyarlathotep") $ \card -> do
            eid <- getRandom
            pure (card, toAttrs $ lookupEnemy (toCardCode card) eid (toCardId card))

          let
            damage = getSum $ foldMap (Sum . enemyHealthDamage . snd) nyarlathoteps
            horror = getSum $ foldMap (Sum . enemySanityDamage . snd) nyarlathoteps
            victory = getSum $ foldMap (Sum . fromMaybe 0 . cdVictoryPoints . toCardDef . fst) nyarlathoteps
            keywords = foldMap (cdKeywords . toCardDef . fst) nyarlathoteps

          fight <- getMax0 <$> foldMapM (fmap Max0 . calculatePrinted . enemyFight . snd) nyarlathoteps
          evade <- getMax0 <$> foldMapM (fmap Max0 . calculatePrinted . enemyEvade . snd) nyarlathoteps
          health <- sumM (calculatePrinted . enemyHealth . snd) nyarlathoteps

          theGreatHall <- selectJust $ locationIs Locations.theOnyxCastle

          trueShape <- genCard Enemies.nyarlathotepTrueShape >>= (\e -> createEnemyWith e Unplaced id)

          pushAll
            [ UpdateEnemy trueShape (EnemyFightActual ?=. Fixed fight)
            , UpdateEnemy trueShape (EnemyEvadeActual ?=. Fixed evade)
            , UpdateEnemy trueShape (EnemyHealthActual ?=. Fixed health)
            , UpdateEnemy trueShape (EnemyHealthDamage =. damage)
            , UpdateEnemy trueShape (EnemySanityDamage =. horror)
            ]

          gameModifier attrs trueShape (GainVictory victory)
          gameModifiers attrs trueShape (map AddKeyword $ toList keywords)
          enemyMoveTo attrs trueShape theGreatHall
        else push R1
      advanceActDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lookAt
        iid
        (attrs.ability 1)
        EncounterDeckTarget
        [(FromTopOfDeck 3, DiscardRest)]
        (basic $ CardWithKeyword Keyword.Hidden)
        (DrawAllFound iid)
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advanceVia #other attrs attrs
      pure a
    _ -> TruthAndLies <$> liftRunMessage msg attrs

module Arkham.Treachery.Cards.RealityAcid5U21 (
  realityAcid5U21,
  realityAcid5U21Effect,
  RealityAcid5U21 (..),
)
where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetCard, AssetCardsUnderneath, AssetTokens))
import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Deck qualified as Deck
import Arkham.Effect.Import
import Arkham.Enemy.Types (Field (EnemyCard))
import Arkham.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Investigator.Types (Field (InvestigatorDeck, InvestigatorDiscard, InvestigatorHand))
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Phase
import Arkham.Projection
import Arkham.RequestedChaosTokenStrategy
import Arkham.SkillTest.Base
import Arkham.Strategy
import Arkham.Token
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RealityAcid5U21 = RealityAcid5U21 TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

realityAcid5U21 :: TreacheryCard RealityAcid5U21
realityAcid5U21 = treachery RealityAcid5U21 Cards.realityAcid5U21

instance RunMessage RealityAcid5U21 where
  runMessage msg t@(RealityAcid5U21 attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      mRavenous <- selectOne $ assetIs Assets.ravenousControlledHunger
      for_ mRavenous $ \ravenous -> do
        push $ Flip iid (toSource attrs) (toTarget ravenous)
      push $ RequestChaosTokens (toSource attrs) (Just iid) (Reveal 1) SetAside
      push $ ResetChaosTokens (toSource attrs)
      pure t
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      case tokens of
        [] -> pure ()
        [x] -> push $ ForTarget (ChaosTokenFaceTarget x.face) msg
        xs -> do
          push $ FocusChaosTokens xs
          chooseOneAtATime
            iid
            [targetLabel x [ForTarget (ChaosTokenFaceTarget x.face) msg] | x <- xs]
      pure t
    ForTarget (ChaosTokenFaceTarget face) (RequestedChaosTokens (isSource attrs -> True) (Just iid) _) -> do
      let again = push $ RequestChaosTokens (toSource attrs) (Just iid) (Reveal 1) SetAside
      case face of
        ElderSign -> do
          enemies <- select $ NonEliteEnemy <> enemyAtLocationWith iid <> not_ EnemyWithVictory
          treacheries <- select $ TreacheryAt (locationWithInvestigator iid) <> not_ TreacheryWithVictory
          if notNull enemies || notNull treacheries
            then chooseOneM iid do
              for_ enemies \enemy -> do
                targeting enemy do
                  card <- field EnemyCard enemy
                  push $ RemoveFromPlay (toSource enemy)
                  push $ Devoured iid card

              for_ treacheries \treachery' -> do
                targeting treachery' do
                  card <- field TreacheryCard treachery'
                  push $ RemoveFromPlay (toSource treachery')
                  push $ Devoured iid card
            else again
        PlusOne -> do
          investigators <- select $ affectsOthers Anyone
          cs <- concatMapM (fieldMap InvestigatorDiscard (take 1)) investigators
          if (null cs)
            then again
            else for_ cs \c -> do
              obtainCard c
              push $ Devoured iid (toCard c)
        Zero -> do
          investigators <- select $ affectsOthers Anyone
          cs <- concatMapM (fieldMap InvestigatorDeck (take 1 . unDeck)) investigators
          if (null cs)
            then again
            else for_ cs \c -> do
              obtainCard c
              push $ Devoured iid (toCard c)
        MinusOne -> do
          hand <- field InvestigatorHand iid
          case nonEmpty hand of
            Nothing -> again
            Just cs -> do
              c <- sample cs
              obtainCard c
              push $ Devoured iid (toCard c)
        MinusTwo -> do
          assets <-
            select
              $ AssetNonStory
              <> AssetControlledBy (affectsOthers $ colocatedWith iid)
              <> NonWeaknessAsset
              <> not_ PermanentAsset
              <> AssetCanLeavePlayByNormalMeans
          if null assets
            then again
            else do
              chooseOneM iid do
                for_ assets \asset -> do
                  targeting asset do
                    c <- field AssetCard asset
                    push $ RemovedFromPlay (toSource asset)
                    push $ Devoured iid c
        MinusThree -> do
          cs <-
            select
              $ oneOf
                [ CardIsAttachedToLocation (locationWithInvestigator iid) <> basic IsPlayerCard
                , CardIsAttachedToEncounterCardAt (locationWithInvestigator iid) <> basic IsPlayerCard
                ]
          if (null cs)
            then again
            else for_ cs \c -> do
              obtainCard c
              push $ Devoured iid (toCard c)
        MinusFour -> do
          investigators <- select $ affectsOthers Anyone
          cs <- concatMapM (fieldMap InvestigatorDeck (take 2 . unDeck)) investigators
          if (null cs)
            then again
            else for_ cs \c -> do
              obtainCard c
              push $ Devoured iid (toCard c)
        MinusFive -> do
          hand <- field InvestigatorHand iid
          if null hand
            then again
            else chooseNM iid 3 do
              for_ hand \c -> do
                targeting c do
                  obtainCard c
                  push $ Devoured iid (toCard c)
        MinusSix -> do
          investigators <- select $ affectsOthers Anyone
          cs <- concatMapM (fieldMap InvestigatorHand (filterCards EventType)) investigators
          if (null cs)
            then again
            else for_ cs \c -> do
              obtainCard c
              push $ Devoured iid (toCard c)
        MinusSeven -> do
          investigators <- select $ affectsOthers Anyone
          groups <- for investigators \investigator -> do
            select $ assetInPlayAreaOf investigator <> NonWeaknessAsset <> AssetNonStory
          chooseOneFromEachM iid
            $ flip map groups \assets -> do
              for_ assets \asset -> do
                targeting asset do
                  c <- field AssetCard asset
                  push $ RemovedFromPlay (toSource asset)
                  push $ Devoured iid c
        MinusEight -> do
          investigators <- select $ affectsOthers Anyone
          signatures <- forMaybeM investigators \investigator -> do
            inDeck <- select $ inDeckOf investigator <> basic (SignatureCard <> NonWeakness)
            inHand <- select $ inHandOf investigator <> basic (SignatureCard <> NonWeakness)
            inDiscard <- select $ inDiscardOf investigator <> basic (SignatureCard <> NonWeakness)
            inPlay <- select $ inPlayAreaOf investigator <> basic (SignatureCard <> NonWeakness)

            let choices = inDeck <> inHand <> inDiscard <> inPlay

            pure
              $ if null choices
                then Nothing
                else
                  Just
                    $ ( investigator
                      , [ targetLabel x
                          $ [ObtainCard x, Devoured iid (toCard x)]
                          <> [ShuffleDeck (Deck.InvestigatorDeck investigator) | x `elem` inDeck]
                        | x <- inDeck <> inHand <> inDiscard <> inPlay
                        ]
                      )

          chooseOneM iid do
            for signatures \(investigator, choices) -> do
              targeting investigator do
                chooseOne iid choices
        Skull -> do
          ravenous <- selectJust $ assetIs Assets.ravenousUncontrolledHunger
          cards <- field AssetCardsUnderneath ravenous
          case nonEmpty cards of
            Nothing -> again
            Just cs -> do
              c <- sample cs
              obtainCard c
              push $ Devoured iid (toCard c)
        Cultist ->
          createCardEffect Cards.realityAcid5U21 (effectMetaTarget $ ChaosTokenFaceTarget Cultist) attrs iid
        Tablet ->
          createCardEffect Cards.realityAcid5U21 (effectMetaTarget $ ChaosTokenFaceTarget Tablet) attrs iid
        ElderThing -> do
          let countRelevant ts = sum $ map (`countTokens` ts) [Evidence, Supply, Ammo, Charge, Secret]
          let mtchr =
                AssetAt (locationWithInvestigator iid)
                  <> oneOf (map AssetWithUses [Evidence, Supply, Ammo, Charge, Secret])
          most <- fieldMaxBy AssetTokens countRelevant mtchr
          assets <- filterM (fmap (== most) . fieldMap AssetTokens countRelevant) =<< select mtchr
          if null assets
            then again
            else do
              chooseOneM iid do
                for_ assets \asset -> do
                  targeting asset do
                    c <- field AssetCard asset
                    push $ RemovedFromPlay (toSource asset)
                    push $ Devoured iid c
        AutoFail -> push $ RequestChaosTokens (toSource attrs) (Just iid) (Reveal 3) SetAside
        _ -> again
      pure t
    _ -> RealityAcid5U21 <$> liftRunMessage msg attrs

newtype RealityAcid5U21Effect = RealityAcid5U21Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

realityAcid5U21Effect :: EffectArgs -> RealityAcid5U21Effect
realityAcid5U21Effect = cardEffect RealityAcid5U21Effect Cards.realityAcid5U21

instance HasModifiersFor RealityAcid5U21Effect where
  getModifiersFor (CardIdTarget x) (RealityAcid5U21Effect attrs) = maybeModified attrs do
    InvestigatorTarget iid <- pure attrs.target
    ChaosTokenFaceTarget face <- hoistMaybe attrs.metaTarget
    case face of
      Cultist -> do
        st <- MaybeT getSkillTest
        let cardPairs = mapToList $ skillTestCommittedCards st
        (iid', _) <- hoistMaybe $ find (elem x . map toCardId . snd) cardPairs
        liftGuardM $ iid' <=~> colocatedWith iid
        pure [SetAfterPlay $ DevourThis iid]
      Tablet -> do
        liftGuardM
          $ selectAny
          $ EventWithCardId x
          <> EventControlledBy (colocatedWith iid)
        pure [SetAfterPlay $ DevourThis iid]
      _ -> error "invalid face"
  getModifiersFor _ _ = pure []

instance RunMessage RealityAcid5U21Effect where
  runMessage msg e@(RealityAcid5U21Effect attrs) = runQueueT $ case msg of
    Begin InvestigationPhase -> do
      pure . RealityAcid5U21Effect $ attrs & setEffectMeta True
    EndPhase -> do
      if getEffectMetaDefault False attrs
        then disableReturn e
        else pure e
    _ -> RealityAcid5U21Effect <$> liftRunMessage msg attrs

module Arkham.Asset.Assets.TheBeyondBleakNetherworld (theBeyondBleakNetherworld) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types qualified as Field
import Arkham.Helpers.Investigator (canHaveHorrorHealed)
import Arkham.Helpers.Message (createEnemyWithPlacement_)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (Discarded)
import Arkham.Message.Lifted.Choose
import Arkham.Name (toTitle)
import Arkham.Placement
import Arkham.Projection
import Arkham.RequestedChaosTokenStrategy

data Meta = Meta
  { spiritDeck :: [Card]
  , selectedSpirit :: Maybe AssetId
  , selectedEnemySpirit :: Maybe EnemyId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheBeyondBleakNetherworld = TheBeyondBleakNetherworld (AssetAttrs `With` Meta)
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBeyondBleakNetherworld :: AssetCard TheBeyondBleakNetherworld
theBeyondBleakNetherworld =
  asset (TheBeyondBleakNetherworld . (`with` Meta [] Nothing Nothing)) Cards.theBeyondBleakNetherworld

instance HasAbilities TheBeyondBleakNetherworld where
  getAbilities (TheBeyondBleakNetherworld (With attrs _)) =
    [restricted attrs 1 ControlsThis $ forced $ TurnBegins #when You]

instance HasModifiersFor TheBeyondBleakNetherworld where
  getModifiersFor (TheBeyondBleakNetherworld (With a _)) = for_ a.controller \iid -> do
    modifySelect a (AssetAttachedToAsset $ be a) [AsIfUnderControlOf iid, IsSpirit iid, DoNotTakeUpSlots]

instance RunMessage TheBeyondBleakNetherworld where
  runMessage msg a@(TheBeyondBleakNetherworld (With attrs meta)) = runQueueT $ case msg of
    CardEnteredPlay iid card | card.id == attrs.cardId -> do
      field InvestigatorSideDeck iid >>= \case
        Nothing -> pure a
        Just sideDeck -> do
          spiritDeck' <- shuffleM =<< traverse (setOwner iid . toCard) sideDeck
          let bonded = nub $ concatMap (cdBondedWith . toCardDef) spiritDeck'

          for_ bonded \(n, cCode) -> do
            case lookupCardDef cCode of
              Nothing -> error "missing card"
              Just def -> do
                taboo <- field InvestigatorTaboo iid
                cs :: [Card] <- replicateM n (genCard def)
                cs' <- traverse (Arkham.Card.setTaboo taboo <=< setOwner iid) cs
                for_ cs' (push . PlaceInBonded iid)

          pure . TheBeyondBleakNetherworld $ attrs `with` Meta spiritDeck' Nothing Nothing
    UseThisAbility iid (isSource attrs -> True) 1 -> case spiritDeck meta of
      [] -> pure a
      card : rest -> do
        let
          triggerVengefulShade = do
            mVengefulShade <- selectOne $ enemyIs Enemies.vengefulShade
            for_ mVengefulShade \vengefulShade -> chooseOneM iid do
              abilityLabeled
                iid
                (mkAbility (SourceableWithCardCode @CardCode "90053" vengefulShade) 1 $ forced NotAnyWindow)
                nothing
        case card.kind of
          AssetType -> do
            mustDiscard <- select $ AssetWithTitle (toTitle card) <> UniqueAsset
            for_ mustDiscard (toDiscard GameSource)
            triggerVengefulShade
            assetId <- getRandom
            push $ CreateAssetAt assetId card $ AttachedToAsset attrs.id (Just $ InPlayArea iid)
          PlayerEnemyType -> do
            pushM $ createEnemyWithPlacement_ card $ AttachedToAsset attrs.id (Just $ InPlayArea iid)
          EnemyType -> do
            triggerVengefulShade
            pushM $ createEnemyWithPlacement_ card $ AttachedToAsset attrs.id (Just $ InPlayArea iid)
          _ -> error "Invalid card type"
        doStep 1 msg
        pure . TheBeyondBleakNetherworld $ attrs `with` Meta rest Nothing Nothing
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      n <-
        (+)
          <$> selectCount (AssetAttachedToAsset $ be attrs)
          <*> selectCount (EnemyAttachedToAsset $ be attrs)
      when (n >= 4) $ flipOver iid attrs
      pure a
    Flip iid _ (isTarget attrs -> True) -> do
      unless attrs.flipped do
        assets <- select $ AssetAttachedToAsset (be attrs) <> NonWeaknessAsset
        enemies <- select $ EnemyAttachedToAsset (be attrs) <> NonWeaknessEnemy
        handleOneAtATime iid (attrs.ability 1) (map toTarget assets <> map toTarget enemies)
        flipOver iid attrs
      pure . TheBeyondBleakNetherworld . (`with` meta) $ attrs & flippedL %~ not
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (AssetTarget aid) -> do
      push $ RequestChaosTokens (attrs.ability 1) (Just iid) (Reveal 1) SetAside
      push $ ResetChaosTokens (attrs.ability 1)
      pure
        . TheBeyondBleakNetherworld
        $ attrs
        `with` meta {selectedSpirit = Just aid, selectedEnemySpirit = Nothing}
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (EnemyTarget eid) -> do
      push $ RequestChaosTokens (attrs.ability 1) (Just iid) (Reveal 1) SetAside
      push $ ResetChaosTokens (attrs.ability 1)
      pure
        . TheBeyondBleakNetherworld
        $ attrs
        `with` meta {selectedSpirit = Nothing, selectedEnemySpirit = Just eid}
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      let mSpirit = (Left <$> selectedSpirit meta) <|> (Right <$> selectedEnemySpirit meta)
      for_ mSpirit \spirit -> do
        discardSpiritMsgs <- capture $ case spirit of
          Left aid -> toDiscardBy iid attrs aid
          Right eid -> do
            card <- field Field.EnemyCard eid
            push $ RemoveEnemy eid
            push $ ScenarioSpecific "theBeyond:returnSpirit" (toJSON card)
        for_ (nub $ map (.face) tokens) \case
          ElderSign -> do
            canHeal <- canHaveHorrorHealed attrs iid
            chooseOneM iid $ cardI18n $ scope "theBeyondBleakNetherworld" do
              when canHeal $ labeled' "elderSignHealHorror" $ healHorror iid attrs 1
              when (notNull $ spiritDeck meta)
                $ labeled' "elderSignAttachSpirit"
                $ doStep 1 msg
              labeled' "elderSignDoNothing" nothing
          AutoFail -> do
            chooseOneM iid $ cardI18n $ scope "theBeyondBleakNetherworld" do
              labeled' "autoFailTakeDirectDamage" do
                pushAll discardSpiritMsgs
                directDamage iid attrs 1
              labeled' "autoFailTakeDirectHorror" do
                pushAll discardSpiritMsgs
                directHorror iid attrs 1
          Skull -> do
            canHeal <- canHaveHorrorHealed attrs iid
            chooseOneM iid $ cardI18n $ scope "theBeyondBleakNetherworld" do
              if canHeal
                then labeled' "skullDiscardSpiritHealHorror" do
                  pushAll discardSpiritMsgs
                  healHorror iid attrs 1
                else labeled' "skullDiscardSpirit" $ pushAll discardSpiritMsgs
          other | other `elem` [Cultist, ElderThing, Tablet, CurseToken] -> do
            chooseOneM iid $ cardI18n $ scope "theBeyondBleakNetherworld" do
              labeled' "symbolDiscardSpirit" $ pushAll discardSpiritMsgs
              labeled' "symbolTakeDirectDamage" $ directDamage iid attrs 1
          _ -> do
            chooseOneM iid $ cardI18n $ scope "theBeyondBleakNetherworld" do
              labeled' "otherDiscardSpirit" $ pushAll discardSpiritMsgs
      pure . TheBeyondBleakNetherworld $ attrs `with` meta {selectedSpirit = Nothing, selectedEnemySpirit = Nothing}
    DoStep 1 (RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) _) -> do
      case spiritDeck meta of
        [] -> pure a
        card : rest -> do
          case card.kind of
            AssetType -> do
              assetId <- getRandom
              push $ CreateAssetAt assetId card $ AttachedToAsset attrs.id (Just $ InPlayArea iid)
            t | t `elem` [PlayerEnemyType, EnemyType] -> do
              pushM $ createEnemyWithPlacement_ card $ AttachedToAsset attrs.id (Just $ InPlayArea iid)
            _ -> error "Invalid card type"
          pure . TheBeyondBleakNetherworld $ attrs `with` Meta rest Nothing Nothing
    ScenarioSpecific "theBeyond:addToSpiritDeck" v -> do
      spiritDeck' <- shuffleM (spiritDeck meta <> toResult v)
      pure . TheBeyondBleakNetherworld $ attrs `with` meta {spiritDeck = spiritDeck'}
    ScenarioSpecific "theBeyond:banishTop" _ -> case spiritDeck meta of
      [] -> pure a
      card : rest -> do
        send $ format card <> " is \"banished\""
        pure . TheBeyondBleakNetherworld $ attrs `with` meta {spiritDeck = rest <> [card]}
    ScenarioSpecific "theBeyond:returnSpirit" v -> do
      pure . TheBeyondBleakNetherworld $ attrs `with` meta {spiritDeck = spiritDeck meta <> [toResult v]}
    Discarded _ _ card@(PlayerCard pc) -> case attrs.controller of
      Nothing -> pure a
      Just iid ->
        field InvestigatorSideDeck iid >>= \case
          Just cards | pc `elem` cards -> do
            obtainCard card
            pure . TheBeyondBleakNetherworld $ attrs `with` meta {spiritDeck = spiritDeck meta <> [card]}
          _ -> pure a
    _ -> TheBeyondBleakNetherworld . (`with` meta) <$> liftRunMessage msg attrs

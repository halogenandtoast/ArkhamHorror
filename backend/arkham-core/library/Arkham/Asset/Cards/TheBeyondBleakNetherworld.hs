module Arkham.Asset.Cards.TheBeyondBleakNetherworld (
  theBeyondBleakNetherworld,
  TheBeyondBleakNetherworld (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Investigator (canHaveHorrorHealed)
import Arkham.Helpers.Message (createEnemyWithPlacement_)
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (Discarded)
import Arkham.Message.Lifted.Choose
import Arkham.Name (toTitle)
import Arkham.Placement
import Arkham.Projection
import Arkham.RequestedChaosTokenStrategy

data Meta = Meta {spiritDeck :: [Card], selectedSpirit :: Maybe AssetId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheBeyondBleakNetherworld = TheBeyondBleakNetherworld (AssetAttrs `With` Meta)
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBeyondBleakNetherworld :: AssetCard TheBeyondBleakNetherworld
theBeyondBleakNetherworld = asset (TheBeyondBleakNetherworld . (`with` Meta [] Nothing)) Cards.theBeyondBleakNetherworld

instance HasAbilities TheBeyondBleakNetherworld where
  getAbilities (TheBeyondBleakNetherworld (With attrs _)) =
    [restrictedAbility attrs 1 ControlsThis $ forced $ TurnBegins #when You]

instance HasModifiersFor TheBeyondBleakNetherworld where
  getModifiersFor (AssetTarget aid) (TheBeyondBleakNetherworld (With a _)) | aid /= a.id = do
    maybeModified a do
      iid <- MaybeT $ field AssetController a.id
      AttachedToAsset aid' _ <- lift $ field AssetPlacement aid
      guard $ aid' == toId a
      pure
        [ AsIfUnderControlOf iid
        , IsSpirit iid
        ]
  getModifiersFor _ _ = pure []

instance RunMessage TheBeyondBleakNetherworld where
  runMessage msg a@(TheBeyondBleakNetherworld (With attrs meta)) = runQueueT $ case msg of
    CardEnteredPlay iid card | card.id == attrs.cardId -> do
      field InvestigatorSideDeck iid >>= \case
        Nothing -> pure a
        Just sideDeck -> do
          spiritDeck' <- shuffleM $ map toCard sideDeck
          pure . TheBeyondBleakNetherworld $ attrs `with` Meta spiritDeck' Nothing
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      case spiritDeck meta of
        [] -> pure a
        card : rest -> do
          case card.kind of
            AssetType -> do
              mustDiscard <- select $ AssetWithTitle (toTitle card) <> UniqueAsset
              for_ mustDiscard \remove -> toDiscard GameSource remove
              mVengefulShade <- selectOne $ enemyIs Enemies.vengefulShade
              for_ mVengefulShade \vengefulShade -> do
                chooseOne
                  iid
                  [ AbilityLabel
                      iid
                      (mkAbility (SourceableWithCardCode @CardCode "90053" vengefulShade) 1 $ forced NotAnyWindow)
                      []
                      []
                      []
                  ]
              assetId <- getRandom
              push $ CreateAssetAt assetId card $ AttachedToAsset attrs.id (Just $ InPlayArea iid)
            PlayerEnemyType -> do
              pushM $ createEnemyWithPlacement_ card $ AttachedToAsset attrs.id (Just $ InPlayArea iid)
            _ -> error "Invalid card type"
          doStep 1 msg
          pure . TheBeyondBleakNetherworld $ attrs `with` Meta rest Nothing
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
        handleOneAtATime iid (attrs.ability 1) assets
        flipOver iid attrs
      pure . TheBeyondBleakNetherworld . (`with` meta) $ attrs & flippedL %~ not
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (AssetTarget aid) -> do
      push $ RequestChaosTokens (attrs.ability 1) (Just iid) (Reveal 1) SetAside
      push $ ResetChaosTokens (attrs.ability 1)
      pure . TheBeyondBleakNetherworld $ attrs `with` Meta (spiritDeck meta) (Just aid)
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      for_ (selectedSpirit meta) \aid -> do
        for_ (map (.face) tokens) \case
          ElderSign -> do
            canHeal <- canHaveHorrorHealed attrs iid
            chooseOrRunOneM iid do
              when canHeal $ labeled "Heal 1 horror" $ healHorror iid attrs 1
              when (notNull $ spiritDeck meta)
                $ labeled "Attach the top card of the spirit deck to The Beyond, as a spirit"
                $ doStep 1 msg
              labeled "Do nothing" nothing
          AutoFail -> do
            toDiscardBy iid attrs aid
            chooseOneM iid do
              labeled "Take 1 direct damage" $ directDamage iid attrs 1
              labeled "Take 1 direct horror" $ directHorror iid attrs 1
          Skull -> do
            toDiscardBy iid attrs aid
            whenM (canHaveHorrorHealed attrs iid) do
              healHorror iid attrs 1
          other | other `elem` [Cultist, ElderThing, Tablet, CurseToken] -> do
            chooseOneM iid do
              labeled "Discard this spirit" $ toDiscardBy iid attrs aid
              labeled "Take 1 direct damage" $ directDamage iid attrs 1
          _ -> toDiscardBy iid attrs aid
      pure a
    DoStep 1 (RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) _) -> do
      case spiritDeck meta of
        [] -> pure a
        card : rest -> do
          assetId <- getRandom
          push $ CreateAssetAt assetId card $ AttachedToAsset attrs.id (Just $ InPlayArea iid)
          pure . TheBeyondBleakNetherworld $ attrs `with` Meta rest Nothing
    Discarded _ _ card@(PlayerCard pc) -> do
      case attrs.controller of
        Nothing -> pure a
        Just iid ->
          field InvestigatorSideDeck iid >>= \case
            Just cards
              | pc `elem` cards ->
                  pure . TheBeyondBleakNetherworld $ attrs `with` Meta (spiritDeck meta <> [card]) Nothing
            _ -> pure a
    _ -> TheBeyondBleakNetherworld . (`with` meta) <$> liftRunMessage msg attrs

module Arkham.Types.Act.Cards.Fold
  ( Fold(..)
  , fold
  ) where

import Arkham.Prelude hiding (fold)

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Action
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

newtype Fold = Fold ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fold :: ActCard Fold
fold = act (3, A) Fold Cards.fold Nothing

instance ActionRunner env => HasAbilities env Fold where
  getAbilities iid window@(Window Timing.When NonFast) (Fold attrs) =
    withBaseAbilities iid window attrs $ do
      investigatorLocationId <- getId @LocationId iid
      maid <- selectOne (assetIs Cards.peterClover)
      case maid of
        Nothing -> pure []
        Just aid -> do
          miid <- fmap unOwnerId <$> getId aid
          assetLocationId <- getId aid
          pure
            [ mkAbility
                (ProxySource (AssetSource aid) (toSource attrs))
                1
                (ActionAbility (Just Parley) $ ActionCost 1)
            | isNothing miid && Just investigatorLocationId == assetLocationId
            ]
  getAbilities i window (Fold x) = getAbilities i window x

instance ActRunner env => RunMessage env Fold where
  runMessage msg a@(Fold attrs@ActAttrs {..}) = case msg of
    InvestigatorResigned _ -> do
      investigatorIds <- getSet @InScenarioInvestigatorId ()
      a <$ when
        (null investigatorIds)
        (push $ AdvanceAct actId (toSource attrs))
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      resignedCardCodes <- map unResignedCardCode <$> getList ()
      a <$ if "02079" `elem` resignedCardCodes
        then push (ScenarioResolution $ Resolution 3)
        else push (ScenarioResolution $ Resolution 1)
    UseCardAbility iid (ProxySource _ source) _ 1 _
      | isSource attrs source && actSequence == Act 3 A -> do
        maid <- selectOne (assetIs Cards.peterClover)
        case maid of
          Nothing -> error "this ability should not be able to be used"
          Just aid -> a <$ push
            (BeginSkillTest
              iid
              source
              (AssetTarget aid)
              (Just Parley)
              SkillWillpower
              3
            )
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source && actSequence == Act 3 A -> do
        maid <- selectOne (assetIs Cards.peterClover)
        case maid of
          Nothing -> error "this ability should not be able to be used"
          Just aid -> do
            currentClueCount <- unClueCount <$> getCount aid
            requiredClueCount <- getPlayerCountValue (PerPlayer 1)
            push (PlaceClues (AssetTarget aid) 1)
            a <$ when
              (currentClueCount + 1 >= requiredClueCount)
              (push $ TakeControlOfAsset iid aid)
    _ -> Fold <$> runMessage msg attrs

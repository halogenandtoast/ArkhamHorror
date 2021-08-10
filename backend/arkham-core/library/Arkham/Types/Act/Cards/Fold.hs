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
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

newtype Fold = Fold ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

fold :: ActCard Fold
fold = act (3, A) Fold Cards.fold Nothing

instance HasActions Fold where
  getActions (Fold attrs) =
    restrictedAbility
        (AssetProxySource
          (assetIs Cards.peterClover <> AssetIsUnowned)
          (toSource attrs)
        )
        1
        OnSameLocation
        (ActionAbility (Just Parley) $ ActionCost 1)
      : getActions attrs

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

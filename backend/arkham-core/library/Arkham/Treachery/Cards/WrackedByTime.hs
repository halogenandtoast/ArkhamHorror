module Arkham.Treachery.Cards.WrackedByTime
  ( wrackedByTime
  , WrackedByTime(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Types ( Field (AssetOwner) )
import Arkham.Classes
import Arkham.Deck
import Arkham.Id
import Arkham.Matcher hiding (EncounterDeck)
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Trait ( Trait (Shattered) )
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Metadata = Metadata { damagedAssets :: HashSet AssetId }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype WrackedByTime = WrackedByTime (TreacheryAttrs `With` Metadata)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wrackedByTime :: TreacheryCard WrackedByTime
wrackedByTime =
  treachery (WrackedByTime . (`with` Metadata mempty)) Cards.wrackedByTime

instance RunMessage WrackedByTime where
  runMessage msg t@(WrackedByTime (attrs `With` meta)) = case msg of
    Revelation iid source | isSource attrs source -> do
      others <-
        selectList
        $ InvestigatorAt (LocationWithTrait Shattered)
        <> NotInvestigator (InvestigatorWithId iid)
      pushAll
        $ RevelationSkillTest iid (toSource attrs) SkillWillpower 3
        : [ RevelationSkillTest iid' (toSource attrs) SkillWillpower 3
          | iid' <- others
          ]
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        push $ InvestigatorAssignDamage iid (toSource attrs) DamageAny 2 0
        pure t
    AssetDamage aid (isSource attrs -> True) _ _ ->
      pure . WrackedByTime $ attrs `with` Metadata
        (insertSet aid $ damagedAssets meta)
    After (Revelation _ (isSource attrs -> True)) -> do
      assets <-
        selectWithField AssetOwner $ AssetOneOf $ map AssetWithId $ setToList
          (damagedAssets meta)
      pushAll
        [ ShuffleIntoDeck deck (AssetTarget aid)
        | (aid, mowner) <- assets
        , let deck = maybe EncounterDeck InvestigatorDeck mowner
        ]
      pure t
    _ -> WrackedByTime . (`with` meta) <$> runMessage msg attrs

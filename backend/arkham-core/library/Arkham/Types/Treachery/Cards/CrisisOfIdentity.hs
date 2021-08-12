module Arkham.Types.Treachery.Cards.CrisisOfIdentity
  ( crisisOfIdentity
  , CrisisOfIdentity(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card.CardDef
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype CrisisOfIdentity = CrisisOfIdentity TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crisisOfIdentity :: TreacheryCard CrisisOfIdentity
crisisOfIdentity = treachery CrisisOfIdentity Cards.crisisOfIdentity

instance
  ( HasSet SkillId env SkillMatcher
  , HasSet EventId env EventMatcher
  , HasSet ClassSymbol env InvestigatorId
  , TreacheryRunner env
  )
  => RunMessage env CrisisOfIdentity where
  runMessage msg t@(CrisisOfIdentity attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      roles <- getSetList iid
      t <$ case roles of
        [] -> error "role has to be set"
        role : _ -> do
          assets <- selectList
            (AssetOwnedBy You <> AssetWithClass role <> DiscardableAsset)
          events <- getSetList (EventOwnedBy iid <> EventWithClass role)
          skills <- getSetList (SkillOwnedBy iid <> SkillWithClass role)
          pushAll
            $ [ Discard $ AssetTarget aid | aid <- assets ]
            <> [ Discard $ EventTarget eid | eid <- events ]
            <> [ Discard $ SkillTarget sid | sid <- skills ]
            <> [ DiscardTopOfDeck iid 1 (Just $ toTarget attrs)
               , Discard (toTarget attrs)
               ]
    DiscardedTopOfDeck iid [card] target | isTarget attrs target -> do
      t <$ push
        (SetRole iid $ fromMaybe Neutral $ cdClassSymbol $ toCardDef card)
    _ -> CrisisOfIdentity <$> runMessage msg attrs

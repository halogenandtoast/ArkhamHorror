module Arkham.Effect.Effects.WilliamYorick
  ( WilliamYorick(..)
  , williamYorick
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Attrs
import Arkham.Id
import Arkham.Message
import Arkham.Modifier
import Arkham.Target

newtype WilliamYorick = WilliamYorick EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

williamYorick :: EffectArgs -> WilliamYorick
williamYorick = WilliamYorick . uncurry4 (baseAttrs "03005")

instance HasModifiersFor env WilliamYorick

instance
  ( HasList DiscardedPlayerCard env InvestigatorId
  , HasModifiersFor env ()
  )
  => RunMessage env WilliamYorick where
  runMessage msg e@(WilliamYorick attrs) = case msg of
    PassedSkillTest _ _ _ SkillTestInitiatorTarget{} _ _ ->
      case effectTarget attrs of
        InvestigatorTarget iid -> do
          modifiers' <- getModifiers (toSource attrs) (InvestigatorTarget iid)
          if CardsCannotLeaveYourDiscardPile `elem` modifiers'
            then pure e
            else do
              discards <- map unDiscardedPlayerCard <$> getList iid
              e <$ when
                (notNull discards)
                (push $ chooseOne
                  iid
                  (Done "Do not return card to hand"
                  : [ TargetLabel
                        (CardIdTarget $ toCardId card)
                        [AddToHand iid $ PlayerCard card]
                    | card <- discards
                    ]
                  )
                )
        _ -> pure e
    SkillTestEnds _ -> e <$ push (DisableEffect $ effectId attrs)
    _ -> WilliamYorick <$> runMessage msg attrs

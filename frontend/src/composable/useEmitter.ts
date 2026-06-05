import { getCurrentInstance } from 'vue'

export default function useEmitter() {
    const internalInstance = getCurrentInstance(); 
    if (!internalInstance) throw new Error('useEmitter must be called inside setup')
    const emitter = internalInstance.appContext.config.globalProperties.emitter

    return emitter;
}
